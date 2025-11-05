package ru.innopolis.interpreter.syntax.analyzer.semantic

import ru.innopolis.interpreter.exception.SemanticCheckException
import ru.innopolis.interpreter.lexer.Code
import ru.innopolis.interpreter.syntax.analyzer.tree.expression._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.TypeCheck
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.indicator.TypeIndicator
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.indicator.TypeIndicator.{ArrayType, BoolType, FuncType, TupleType}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement._
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.assignment.{ArrayElementAssignment, VariableAssignment}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop.{CollectionLoop, Loop, RangeLoop, WhileLoop}

import scala.collection.mutable

class SemanticCheckAnalyzer {

  private val scopes: mutable.Stack[mutable.Set[String]] = mutable.Stack(mutable.Set.empty[String])

  private var inFunction: Boolean = false
  private var inLoop: Boolean = false

  def analyze(block: CodeBlock): Unit = checkCodeBlock(block)

  private def checkCodeBlock(block: CodeBlock): Unit = {
    enterScope()
    block.statements.foreach(checkStatement)
    exitScope()
  }

  private def checkStatement(stmt: Statement): Unit = stmt match {
    case VariableDeclaration(name, expr) =>
      checkExpression(expr)
      declareVariable(name)

    case VariableAssignment(name, expr) =>
      checkExpression(expr)
      if (!isDeclared(name))
        throw new SemanticCheckException(s"Variable '$name' was not declared before")

    case ArrayElementAssignment(target, index, expr) =>
      checkExpression(target)
      checkExpression(index)
      checkExpression(expr)

    case ExpressionStatement(expr) =>
      checkExpression(expr)

    case ReturnStatement(exprOpt) =>
      if (!inFunction) throw new SemanticCheckException("`return` is outside of function")
      exprOpt.foreach(checkExpression)

    case PrintStatement(exprs) =>
      exprs.foreach(checkExpression)

    case IfStatement(cond, thenBlock, elseBlock) =>
      checkExpression(cond)
      checkCodeBlock(thenBlock)
      elseBlock.foreach(checkCodeBlock)

    case WhileLoop(cond, body) =>
      checkExpression(cond)
      val prev = inLoop
      inLoop = true
      checkCodeBlock(body)
      inLoop = prev

    case CollectionLoop(varName, coll, body) =>
      checkExpression(coll)
      enterScope()
      declareVariable(varName)
      val prev = inLoop
      inLoop = true
      checkCodeBlock(body)
      inLoop = prev
      exitScope()

    case RangeLoop(optVar, from, to, body) =>
      checkExpression(from)
      checkExpression(to)
      optVar.foreach { v =>
        enterScope()
        declareVariable(v)
        val prev = inLoop
        inLoop = true
        checkCodeBlock(body)
        inLoop = prev
        exitScope()
      }
      if (optVar.isEmpty) {
        val prev = inLoop
        inLoop = true
        checkCodeBlock(body)
        inLoop = prev
      }

    case Loop(body) =>
      val prev = inLoop
      inLoop = true
      checkCodeBlock(body)
      inLoop = prev

    case ExitStatement() =>
      if (!inLoop) throw new SemanticCheckException("`exit` is outside of loop")
  }

  private def checkExpression(expr: Expression): Option[TypeIndicator] = expr match {
    case Literal(v) => v match {
      case _: Int | _: Long => Some(TypeIndicator.IntType)
      case _: Double => Some(TypeIndicator.RealType)
      case _: Boolean => Some(TypeIndicator.BoolType)
      case _: String => Some(TypeIndicator.StringType)
      case t => throw new SemanticCheckException(s"Unknown type in expression $expr: ${t.getClass.getSimpleName} ")
    }

    case Variable(name) =>
      validateDeclaration(name)
      None

    case Unary(_, inner) => checkExpression(inner)

    case Binary(op, left, right) =>
      val firstTypeOption = checkExpression(left)
      val secondTypeOption = checkExpression(right)

      if (!(firstTypeOption.nonEmpty && secondTypeOption.nonEmpty))
        None
      else
        Some(inferType(firstTypeOption.get, secondTypeOption.get, op))

    case TypeCheck(inner, _) =>
      checkExpression(inner)
      Some(BoolType)

    case ArrayLiteral(elements) =>
      elements.foreach(checkExpression)
      Some(ArrayType)

    case TupleLiteral(elems) =>
      elems.foreach { case TupleEntry(_, e) => checkExpression(e) }
      Some(TupleType)

    case FunctionLiteral(args, body) =>
      enterScope()
      args.foreach(v => declareVariable(v.value))
      val prevInFunction = inFunction
      inFunction = true
      checkCodeBlock(body)
      inFunction = prevInFunction
      exitScope()
      Some(FuncType)

    case LambdaLiteral(args, expr) =>
      enterScope()
      args.foreach(v => declareVariable(v.value))
      val prevInFunction = inFunction
      inFunction = true
      checkExpression(expr)
      inFunction = prevInFunction
      exitScope()
      Some(FuncType)

    case FunctionCall(funcExpr, args) =>
      checkExpression(funcExpr)
      args.foreach(a => checkExpression(a))
      None


    case ArrayAccess(target, idx) =>
      checkExpression(target)
      checkExpression(idx)

    case TupleFieldAccess(target, _) =>
      checkExpression(target)

    case TupleIndexAccess(target, _) =>
      checkExpression(target)

    case other =>
      throw new SemanticCheckException(s"Unknown expression type: ${other.getClass.getSimpleName}")
  }

  private def enterScope(): Unit = scopes.push(mutable.Set.empty)

  private def exitScope(): Unit = if (scopes.nonEmpty) scopes.pop()

  private def declareVariable(name: String): Unit = scopes.head.add(name)

  private def isDeclared(name: String): Boolean = scopes.exists(_.contains(name))

  private def validateDeclaration(name: String): Unit = {
    if (!isDeclared(name)) {
      throw new SemanticCheckException(s"Identifier '$name' not declared")
    }
  }

  private def inferType(t1: TypeIndicator, t2: TypeIndicator, op: Code): TypeIndicator = {
    import TypeIndicator._
    import ru.innopolis.interpreter.lexer.Code._

    (op, t1, t2) match {

      //arithmetic
      case (PLUS | MINUS | MULTIPLICATION | DIVISION, IntType, IntType) =>
        if (op == DIVISION) RealType else IntType
      case (PLUS | MINUS | MULTIPLICATION | DIVISION, RealType, RealType) =>
        RealType
      case (PLUS | MINUS | MULTIPLICATION | DIVISION, IntType, RealType) |
           (PLUS | MINUS | MULTIPLICATION | DIVISION, RealType, IntType) =>
        RealType

      //string concat
      case (PLUS, StringType, StringType) =>
        StringType

      //tuple concat
      case (PLUS, TupleType, TupleType) =>
        TupleType

      //array concat
      case (PLUS, ArrayType, ArrayType) =>
        ArrayType

      //boolean
      case (LESS | MORE | LESS_OR_EQUAL | MORE_OR_EQUAL | EQUAL | NOT_EQUAL, IntType, IntType) |
           (LESS | MORE | LESS_OR_EQUAL | MORE_OR_EQUAL | EQUAL | NOT_EQUAL, RealType, RealType) |
           (LESS | MORE | LESS_OR_EQUAL | MORE_OR_EQUAL | EQUAL | NOT_EQUAL, IntType, RealType) |
           (LESS | MORE | LESS_OR_EQUAL | MORE_OR_EQUAL | EQUAL | NOT_EQUAL, RealType, IntType) =>
        BoolType

      //logican
      case (AND | OR | XOR, BoolType, BoolType) =>
        BoolType

      //eq
      case (EQUAL | NOT_EQUAL, BoolType, BoolType) =>
        BoolType
      case (EQUAL | NOT_EQUAL, StringType, StringType) =>
        BoolType

      //otherwise
      case _ =>
        throw new SemanticCheckException(
          s"Operation '$op' is not allowed between types $t1 and $t2"
        )
    }
  }
}
