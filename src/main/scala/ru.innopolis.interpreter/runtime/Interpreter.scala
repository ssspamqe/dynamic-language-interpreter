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
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
 * Exception used to exit from a loop
 */
private class LoopExitException extends Exception

// Исключение для выхода из функции по return
private class FunctionReturn(val value: Option[Any]) extends Throwable

/**
 * Interpreter that executes the AST directly
 */
class Interpreter {
  private var environment = new Stack()

  def interpret(block: CodeBlock): Unit = {
    environment = new Stack()
    executeBlock(block)
  }

  private def executeBlock(block: CodeBlock): Unit = {
    // просто выполняем операторы в текущем environment (слоями управляют вызывающие)
    block.statements.foreach(executeStatement)
  }

  private def withNewLayer[A](body: => A): A = {
    environment.addLayer()
    try body
    finally environment.popLayer()
  }

  private def executeStatement(stmt: Statement): Unit = stmt match {
    case PrintStatement(expressions) =>
      val values = expressions.map(e => formatValue(evaluateExpression(e)))
      print(values.mkString(" "))

    case VariableDeclaration(name, expr) =>
      val value = evaluateExpression(expr)
      environment.defineVariable(name, value)

    case VariableAssignment(name, expr) =>
      val value = evaluateExpression(expr)
      environment.setVariable(name, value)

    case ArrayElementAssignment(target, index, value) =>
      val arr = evaluateExpression(target).asInstanceOf[ArrayBuffer[Any]]
      val idx = evaluateExpression(index) match {
        case l: Long => l.toInt
        case i: Int  => i
        case _       => throw new RuntimeException("Array index must be an integer")
      }
      val valValue = evaluateExpression(value)
      // Автоматически расширяем массив если нужно
      if (idx > arr.length) {
        arr ++= ArrayBuffer.fill(idx - arr.length)(0)
      }
      arr(idx - 1) = valValue

    case IfStatement(condition, trueBranch, falseBranch) =>
      val condValue = evaluateExpression(condition) match {
        case b: Boolean => b
        case _          => throw new RuntimeException("Condition must be a boolean")
      }
      if (condValue) {
        withNewLayer { executeBlock(trueBranch) }
      } else {
        falseBranch.foreach(b => withNewLayer { executeBlock(b) })
      }

    case WhileLoop(condition, body) =>
      breakable {
        while (true) {
          val condValue = evaluateExpression(condition) match {
            case b: Boolean => b
            case _          => throw new RuntimeException("While condition must be a boolean")
          }
          if (!condValue) break
          try {
            withNewLayer { executeBlock(body) }
          } catch {
            case _: LoopExitException => break
            case fr: FunctionReturn   => throw fr // пробрасываем return наружу
          }
        }
      }

    case RangeLoop(ident, from, to, body) =>
      val fromValue = evaluateExpression(from) match {
        case l: Long => l
        case i: Int  => i.toLong
        case _       => throw new RuntimeException("Range loop 'from' must be an integer")
      }
      val toValue = evaluateExpression(to) match {
        case l: Long => l
        case i: Int  => i.toLong
        case _       => throw new RuntimeException("Range loop 'to' must be an integer")
      }
      breakable {
        for (i <- fromValue to toValue) {
          try {
            withNewLayer {
              ident.foreach(name => environment.defineVariable(name, i))
              executeBlock(body)
            }
          } catch {
            case _: LoopExitException => break
            case fr: FunctionReturn   => throw fr
          }
        }
      }

    case CollectionLoop(ident, collection, body) =>
      val coll = evaluateExpression(collection)
      breakable {
        coll match {
          case arr: ArrayBuffer[Any] =>
            for (elem <- arr) {
              try {
                withNewLayer {
                  environment.defineVariable(ident, elem)
                  executeBlock(body)
                }
              } catch {
                case _: LoopExitException => break
                case fr: FunctionReturn   => throw fr
              }
            }
          case list: List[Any] =>
            for (elem <- list) {
              try {
                withNewLayer {
                  environment.defineVariable(ident, elem)
                  executeBlock(body)
                }
              } catch {
                case _: LoopExitException => break
                case fr: FunctionReturn   => throw fr
              }
            }
          case _ => throw new RuntimeException("Collection loop requires an array or list")
        }
      }

    case loop: Loop =>
      breakable {
        while (true) {
          try {
            withNewLayer { executeBlock(loop.body) }
          } catch {
            case _: LoopExitException => break
            case fr: FunctionReturn   => throw fr
          }
        }
      }

    case ReturnStatement(expr) =>
      throw new FunctionReturn(expr.map(evaluateExpression))

    case ExitStatement() =>
      throw new LoopExitException()

    case ExpressionStatement(expr) =>
      evaluateExpression(expr)
  }

  private def evaluateExpression(expr: Expression): Any = expr match {
    case Literal(value) => value

    case Variable(name) => environment.getVariable(name)

    case Binary(operation, left, right) =>
      val leftVal  = evaluateExpression(left)
      val rightVal = evaluateExpression(right)
      evaluateBinary(operation, leftVal, rightVal)

    case Unary(operation, right) =>
      val rightVal = evaluateExpression(right)
      evaluateUnary(operation, rightVal)

    case FunctionCall(target, args) =>
      val func      = evaluateExpression(target)
      val argValues = args.map(evaluateExpression)
      callFunction(func, argValues)

    case ArrayAccess(target, index) =>
      val arr = evaluateExpression(target).asInstanceOf[ArrayBuffer[Any]]
      val idx = evaluateExpression(index) match {
        case l: Long => l.toInt
        case i: Int  => i
        case _       => throw new RuntimeException("Array index must be an integer")
      }
      if (idx > arr.length) {
        throw new RuntimeException(s"Array index $idx out of bounds for array of length ${arr.length}")
      }
      arr(idx - 1)

    case ArrayLiteral(elements) =>
      ArrayBuffer.from(elements.map(evaluateExpression))

    case TupleLiteral(elements) =>
      val map   = mutable.Map[String, Any]()
      var index = 1
      for (entry <- elements) {
        val value = evaluateExpression(entry.value)
        entry.key.foreach(key => map(key) = value)
        map(index.toString) = value
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
      val capturedEnv = environment // стек на момент определения функции
      (argValues: List[Any]) => {
        if (argValues.length != args.length) {
          throw new RuntimeException(s"Expected ${args.length} arguments, got ${argValues.length}")
        }
        // создаём окружение вызова, замкнутое на capturedEnv
        val funcEnv   = new Stack(Some(capturedEnv))
        val oldEnvRef = environment
        environment = funcEnv
        // новый слой для параметров
        environment.addLayer()
        for ((arg, value) <- args.zip(argValues)) {
          environment.defineVariable(arg.value, value)
        }
        try {
          try {
            executeBlock(body)
            None
          } catch {
            case fr: FunctionReturn => fr.value.getOrElse(None)
          }
        } finally {
          environment = oldEnvRef
        }
      }

    case LambdaLiteral(args, body) =>
      val capturedEnv = environment
      (argValues: List[Any]) => {
        if (argValues.length != args.length) {
          throw new RuntimeException(s"Expected ${args.length} arguments, got ${argValues.length}")
        }
        val funcEnv   = new Stack(Some(capturedEnv))
        val oldEnvRef = environment
        environment = funcEnv
        environment.addLayer()
        for ((arg, value) <- args.zip(argValues)) {
          environment.defineVariable(arg.value, value)
        }
        try {
          try {
            evaluateExpression(body)
          } catch {
            case fr: FunctionReturn => fr.value.getOrElse(None)
          }
        } finally {
          environment = oldEnvRef
        }
      }

    case TypeCheck(expression, typeIndicator) =>
      val value = evaluateExpression(expression)
      checkType(value, typeIndicator)

    case _ => throw new RuntimeException(s"Unsupported expression: $expr")
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
          case (l: ArrayBuffer[Any], r: ArrayBuffer[Any]) => l ++ r
          case (l: List[Any], r: List[Any]) => l ::: r
          case (l: Map[String, Any], r: Map[String, Any]) => {
            val length = l.map { case (k, v) => k.toIntOption.getOrElse(0) }.max
            l ++ r.map { case (k, v) => (k.toIntOption.map(k => (k + length).toString).getOrElse(k), v) }
          }
          case (l: mutable.Seq[Any], r: mutable.Seq[Any]) => l ++ r
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
      case TypeIndicator.ArrayType => value.isInstanceOf[ArrayBuffer[Any]] || value.isInstanceOf[List[Any]]
      case TypeIndicator.TupleType => value.isInstanceOf[Map[_, _]]
      case TypeIndicator.FuncType => value.isInstanceOf[List[Any] => Any]
    }
  }

  private def formatValue(value: Any, quotes: Boolean = false): String = value match {

    // string
    case s: String if quotes =>
      "\"" + s + "\""
    case s: String if !quotes =>
      s

    // integer / real / boolean
    case n: Number => n.toString
    case b: Boolean => b.toString

    // array
    case arr: ArrayBuffer[Any] =>
      "[" + arr.map(formatValue(_, true)).mkString(", ") + "]"

    // tuple (Map[String, Any])
    case map: Map[_, _] =>
      // сортируем по ключам: сначала имена, потом числовые индексы
      val (named, indexed) = map.toList.partition(_._1.asInstanceOf[String].forall(!_.isDigit))

      val namedSorted   = named.asInstanceOf[List[(String, Any)]].sortBy(_._1)
      val indexedSorted = indexed.asInstanceOf[List[(String, Any)]].sortBy(_._1.toInt)

      val parts =
        (namedSorted ++ indexedSorted).map { case (k, v) => s"$k:=${formatValue(v, true)}" }

      "{" + parts.mkString(", ") + "}"

    // function (print as <function>)
    case f: (List[Any] => Any) =>
      "<function>"

    // anything else
    case other =>
      other.toString
  }

}