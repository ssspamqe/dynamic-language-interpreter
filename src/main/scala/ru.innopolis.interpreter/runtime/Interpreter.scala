package ru.innopolis.interpreter.runtime

import ru.innopolis.interpreter.lexer.Code
import ru.innopolis.interpreter.syntax.analyzer.tree.expression._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.TypeCheck
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.indicator.TypeIndicator
import ru.innopolis.interpreter.syntax.analyzer.tree.statement._
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.assignment.{ArrayElementAssignment, VariableAssignment}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop.{CollectionLoop, Loop, RangeLoop, WhileLoop}

import scala.collection.mutable
import scala.util.control.Breaks._

/**
 * Interpreter that executes the AST directly
 */
class Interpreter {
  private var environment = new Environment()
  private var returnValue: Option[Any] = None
  private var shouldExit: Boolean = false

  def interpret(block: CodeBlock): Unit = {
    environment = new Environment()
    returnValue = None
    shouldExit = false
    executeBlock(block, environment)
  }

  private def executeBlock(block: CodeBlock, env: Environment): Unit = {
    val oldEnv = environment
    environment = env
    try {
      for (stmt <- block.statements) {
        if (shouldExit || returnValue.isDefined) return
        executeStatement(stmt)
      }
    } finally {
      environment = oldEnv
    }
  }

  private def executeStatement(stmt: Statement): Unit = {
    stmt match {
      case PrintStatement(expressions) =>
        val values = expressions.map(evaluateExpression)
        println(values.mkString(" "))

      case VariableDeclaration(name, expr) =>
        val value = evaluateExpression(expr)
        environment.defineVariable(name, value)

      case VariableAssignment(name, expr) =>
        val value = evaluateExpression(expr)
        environment.setVariable(name, value)

      case ArrayElementAssignment(target, index, value) =>
        val arr = evaluateExpression(target).asInstanceOf[Array[Any]]
        val idx = evaluateExpression(index) match {
          case l: Long => l.toInt
          case i: Int => i
          case _ => throw new RuntimeException("Array index must be an integer")
        }
        val valValue = evaluateExpression(value)
        arr(idx) = valValue

      case IfStatement(condition, trueBranch, falseBranch) =>
        val condValue = evaluateExpression(condition) match {
          case b: Boolean => b
          case _ => throw new RuntimeException("Condition must be a boolean")
        }
        if (condValue) {
          executeBlock(trueBranch, environment.createChild())
        } else {
          falseBranch.foreach(executeBlock(_, environment.createChild()))
        }

      case WhileLoop(condition, body) =>
        breakable {
          while (true) {
            if (shouldExit || returnValue.isDefined) break
            val condValue = evaluateExpression(condition) match {
              case b: Boolean => b
              case _ => throw new RuntimeException("While condition must be a boolean")
            }
            if (!condValue) break
            executeBlock(body, environment.createChild())
          }
        }

      case RangeLoop(ident, from, to, body) =>
        val fromValue = evaluateExpression(from) match {
          case l: Long => l
          case i: Int => i.toLong
          case _ => throw new RuntimeException("Range loop 'from' must be an integer")
        }
        val toValue = evaluateExpression(to) match {
          case l: Long => l
          case i: Int => i.toLong
          case _ => throw new RuntimeException("Range loop 'to' must be an integer")
        }
        val childEnv = environment.createChild()
        breakable {
          for (i <- fromValue to toValue) {
            if (shouldExit || returnValue.isDefined) break
            ident.foreach(name => childEnv.defineVariable(name, i))
            executeBlock(body, childEnv)
          }
        }

      case CollectionLoop(ident, collection, body) =>
        val coll = evaluateExpression(collection)
        val childEnv = environment.createChild()
        breakable {
          coll match {
            case arr: Array[Any] =>
              for (elem <- arr) {
                if (shouldExit || returnValue.isDefined) break
                childEnv.defineVariable(ident, elem)
                executeBlock(body, childEnv)
              }
            case list: List[Any] =>
              for (elem <- list) {
                if (shouldExit || returnValue.isDefined) break
                childEnv.defineVariable(ident, elem)
                executeBlock(body, childEnv)
              }
            case _ => throw new RuntimeException("Collection loop requires an array or list")
          }
        }

      case loop: Loop =>
        breakable {
          while (true) {
            if (shouldExit || returnValue.isDefined) break
            executeBlock(loop.body, environment.createChild())
          }
        }

      case ReturnStatement(expr) =>
        returnValue = expr.map(evaluateExpression)

      case ExitStatement() =>
        shouldExit = true

      case ExpressionStatement(expr) =>
        evaluateExpression(expr) // Evaluate but don't use result
    }
  }

  private def evaluateExpression(expr: Expression): Any = {
    expr match {
      case Literal(value) => value

      case Variable(name) => environment.getVariable(name)

      case Binary(operation, left, right) =>
        val leftVal = evaluateExpression(left)
        val rightVal = evaluateExpression(right)
        evaluateBinary(operation, leftVal, rightVal)

      case Unary(operation, right) =>
        val rightVal = evaluateExpression(right)
        evaluateUnary(operation, rightVal)

      case FunctionCall(target, args) =>
        val func = evaluateExpression(target)
        val argValues = args.map(evaluateExpression)
        callFunction(func, argValues)

      case ArrayAccess(target, index) =>
        val arr = evaluateExpression(target).asInstanceOf[Array[Any]]
        val idx = evaluateExpression(index) match {
          case l: Long => l.toInt
          case i: Int => i
          case _ => throw new RuntimeException("Array index must be an integer")
        }
        arr(idx)

      case ArrayLiteral(elements) =>
        elements.map(evaluateExpression).toArray

      case TupleLiteral(elements) =>
        val map = mutable.Map[String, Any]()
        var index = 0
        for (entry <- elements) {
          val value = evaluateExpression(entry.value)
          entry.key match {
            case Some(key) => map(key) = value
            case None => map(index.toString) = value
          }
          index += 1
        }
        map.toMap

      case TupleFieldAccess(target, field) =>
        val tuple = evaluateExpression(target).asInstanceOf[Map[String, Any]]
        tuple.getOrElse(field, throw new RuntimeException(s"Tuple field '$field' not found"))

      case TupleIndexAccess(target, index) =>
        val tuple = evaluateExpression(target).asInstanceOf[Map[String, Any]]
        tuple.getOrElse(index.toString, throw new RuntimeException(s"Tuple index $index not found"))

      case FunctionLiteral(args, body) =>
        (argValues: List[Any]) => {
          if (argValues.length != args.length) {
            throw new RuntimeException(s"Expected ${args.length} arguments, got ${argValues.length}")
          }
          val funcEnv = environment.createChild()
          for ((arg, value) <- args.zip(argValues)) {
            funcEnv.defineVariable(arg.value, value)
          }
          val oldReturn = returnValue
          val oldExit = shouldExit
          returnValue = None
          shouldExit = false
          try {
            executeBlock(body, funcEnv)
            returnValue.getOrElse(None)
          } finally {
            returnValue = oldReturn
            shouldExit = oldExit
          }
        }

      case LambdaLiteral(args, body) =>
        (argValues: List[Any]) => {
          if (argValues.length != args.length) {
            throw new RuntimeException(s"Expected ${args.length} arguments, got ${argValues.length}")
          }
          val funcEnv = environment.createChild()
          for ((arg, value) <- args.zip(argValues)) {
            funcEnv.defineVariable(arg.value, value)
          }
          evaluateExpression(body)
        }

      case Lambda(params, body) =>
        (argValues: List[Any]) => {
          if (argValues.length != params.length) {
            throw new RuntimeException(s"Expected ${params.length} arguments, got ${argValues.length}")
          }
          val funcEnv = environment.createChild()
          for ((param, value) <- params.zip(argValues)) {
            funcEnv.defineVariable(param, value)
          }
          evaluateExpression(body)
        }

      case TypeCheck(expression, typeIndicator) =>
        val value = evaluateExpression(expression)
        checkType(value, typeIndicator)

      case _ => throw new RuntimeException(s"Unsupported expression: $expr")
    }
  }

  private def evaluateBinary(operation: Code, left: Any, right: Any): Any = {
    operation match {
      case Code.PLUS =>
        (left, right) match {
          case (l: Long, r: Long) => l + r
          case (l: Double, r: Double) => l + r
          case (l: Long, r: Double) => l + r
          case (l: Double, r: Long) => l + r
          case (l: String, r: Any) => l + r.toString
          case (l: Any, r: String) => l.toString + r
          case _ => throw new RuntimeException(s"Cannot add $left and $right")
        }

      case Code.MINUS =>
        (left, right) match {
          case (l: Long, r: Long) => l - r
          case (l: Double, r: Double) => l - r
          case (l: Long, r: Double) => l - r
          case (l: Double, r: Long) => l - r
          case _ => throw new RuntimeException(s"Cannot subtract $right from $left")
        }

      case Code.MULTIPLICATION =>
        (left, right) match {
          case (l: Long, r: Long) => l * r
          case (l: Double, r: Double) => l * r
          case (l: Long, r: Double) => l * r
          case (l: Double, r: Long) => l * r
          case _ => throw new RuntimeException(s"Cannot multiply $left and $right")
        }

      case Code.DIVISION =>
        (left, right) match {
          case (l: Long, r: Long) => if (r == 0) throw new RuntimeException("Division by zero") else l / r
          case (l: Double, r: Double) => if (r == 0) throw new RuntimeException("Division by zero") else l / r
          case (l: Long, r: Double) => if (r == 0) throw new RuntimeException("Division by zero") else l / r
          case (l: Double, r: Long) => if (r == 0) throw new RuntimeException("Division by zero") else l / r
          case _ => throw new RuntimeException(s"Cannot divide $left by $right")
        }

      case Code.LESS =>
        compare(left, right) < 0

      case Code.LESS_OR_EQUAL =>
        compare(left, right) <= 0

      case Code.MORE =>
        compare(left, right) > 0

      case Code.MORE_OR_EQUAL =>
        compare(left, right) >= 0

      case Code.EQUAL =>
        left == right

      case Code.NOT_EQUAL =>
        left != right

      case Code.AND =>
        (left, right) match {
          case (l: Boolean, r: Boolean) => l && r
          case _ => throw new RuntimeException("AND operation requires boolean operands")
        }

      case Code.OR =>
        (left, right) match {
          case (l: Boolean, r: Boolean) => l || r
          case _ => throw new RuntimeException("OR operation requires boolean operands")
        }

      case Code.XOR =>
        (left, right) match {
          case (l: Boolean, r: Boolean) => l ^ r
          case _ => throw new RuntimeException("XOR operation requires boolean operands")
        }

      case _ => throw new RuntimeException(s"Unsupported binary operation: $operation")
    }
  }

  private def evaluateUnary(operation: Code, right: Any): Any = {
    operation match {
      case Code.MINUS =>
        right match {
          case l: Long => -l
          case d: Double => -d
          case _ => throw new RuntimeException(s"Cannot negate $right")
        }

      case Code.NOT =>
        right match {
          case b: Boolean => !b
          case _ => throw new RuntimeException("NOT operation requires boolean operand")
        }

      case _ => throw new RuntimeException(s"Unsupported unary operation: $operation")
    }
  }

  private def compare(left: Any, right: Any): Int = {
    (left, right) match {
      case (l: Long, r: Long) => l.compareTo(r)
      case (l: Double, r: Double) => l.compareTo(r)
      case (l: Long, r: Double) => l.toDouble.compareTo(r)
      case (l: Double, r: Long) => l.compareTo(r.toDouble)
      case (l: String, r: String) => l.compareTo(r)
      case _ => throw new RuntimeException(s"Cannot compare $left and $right")
    }
  }

  private def callFunction(func: Any, args: List[Any]): Any = {
    func match {
      case f: (List[Any] => Any) => f(args)
      case _ => throw new RuntimeException(s"Cannot call function: $func")
    }
  }

  private def checkType(value: Any, typeIndicator: TypeIndicator): Boolean = {
    typeIndicator match {
      case TypeIndicator.IntType => value.isInstanceOf[Long] || value.isInstanceOf[Int]
      case TypeIndicator.RealType => value.isInstanceOf[Double] || value.isInstanceOf[Float]
      case TypeIndicator.BoolType => value.isInstanceOf[Boolean]
      case TypeIndicator.StringType => value.isInstanceOf[String]
      case TypeIndicator.NoneType => value == None
      case TypeIndicator.ArrayType => value.isInstanceOf[Array[Any]] || value.isInstanceOf[List[Any]]
      case TypeIndicator.TupleType => value.isInstanceOf[Map[_, _]]
      case TypeIndicator.FuncType => value.isInstanceOf[List[Any] => Any]
    }
  }
}

