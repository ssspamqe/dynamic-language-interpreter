package ru.innopolis.interpreter.syntax.analyzer.semantic

import ru.innopolis.interpreter.syntax.analyzer.tree.expression._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.TypeCheck
import ru.innopolis.interpreter.syntax.analyzer.tree.statement._
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.assignment.{ArrayElementAssignment, VariableAssignment}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop.{CollectionLoop, Loop, RangeLoop, WhileLoop}

import scala.collection.mutable

// Exception for semantic errors
class SemanticException(msg: String) extends RuntimeException(msg)

class SemanticAnalyzer {

  // stack of scopes (name -> either variable type placeholder or function signature)
  private val scopes: mutable.Stack[mutable.Set[String]] = mutable.Stack(mutable.Set.empty[String])

  private var inFunction: Boolean = false
  private var inLoop: Boolean = false

  // entry point
  def analyze(block: CodeBlock): Unit = analyzeCodeBlock(block)

  // analyze code block
  private def analyzeCodeBlock(block: CodeBlock): Unit = {
    enterScope()
    block.statements.foreach(analyzeStatement)
    exitScope()
  }

  // analyze statement
  private def analyzeStatement(stmt: Statement): Unit = stmt match {
    case VariableDeclaration(name, expr) =>
      analyzeExpression(expr)
      declareVariable(name)

    case VariableAssignment(name, expr) =>
      analyzeExpression(expr)
      if (!isDeclared(name))
        throw new SemanticException(s"Variable '$name' not declared before assignment")

    case ArrayElementAssignment(target, index, expr) =>
      analyzeExpression(target)
      analyzeExpression(index)
      analyzeExpression(expr)

    case ExpressionStatement(expr) =>
      analyzeExpression(expr)

    case ReturnStatement(exprOpt) =>
      if (!inFunction) throw new SemanticException("`return` outside function")
      exprOpt.foreach(analyzeExpression)

    case PrintStatement(exprs) =>
      exprs.foreach(analyzeExpression)

    case IfStatement(cond, thenBlock, elseBlock) =>
      analyzeExpression(cond)
      analyzeCodeBlock(thenBlock)
      elseBlock.foreach(analyzeCodeBlock)

    case WhileLoop(cond, body) =>
      analyzeExpression(cond)
      val prev = inLoop
      inLoop = true
      analyzeCodeBlock(body)
      inLoop = prev

    case CollectionLoop(varName, coll, body) =>
      analyzeExpression(coll)
      enterScope()
      declareVariable(varName)
      val prev = inLoop
      inLoop = true
      analyzeCodeBlock(body)
      inLoop = prev
      exitScope()

    case RangeLoop(optVar, from, to, body) =>
      analyzeExpression(from)
      analyzeExpression(to)
      optVar.foreach { v =>
        enterScope()
        declareVariable(v)
        val prev = inLoop
        inLoop = true
        analyzeCodeBlock(body)
        inLoop = prev
        exitScope()
      }
      if (optVar.isEmpty) {
        val prev = inLoop
        inLoop = true
        analyzeCodeBlock(body)
        inLoop = prev
      }

    case Loop(body) =>
      val prev = inLoop
      inLoop = true
      analyzeCodeBlock(body)
      inLoop = prev

    case ExitStatement() =>
      if (!inLoop) throw new SemanticException("`exit` outside loop")
  }

  // analyze expressions
  private def analyzeExpression(expr: Expression): Unit = expr match {
    case Literal(v) => // literals are always valid

    case Variable(name) => validateDeclaration(name)

    case Unary(_, inner) => analyzeExpression(inner)

    case Binary(_, left, right) =>
      analyzeExpression(left)
      analyzeExpression(right)

    case TypeCheck(inner, _) =>
      analyzeExpression(inner)

    case ArrayLiteral(elements) =>
      elements.foreach(analyzeExpression)

    case TupleLiteral(elems) =>
      elems.foreach { case TupleEntry(_, e) => analyzeExpression(e) }

    case FunctionLiteral(args, body) =>
      enterScope()
      args.foreach(v => declareVariable(v.value))
      val prevInFunction = inFunction
      inFunction = true
      analyzeCodeBlock(body)
      inFunction = prevInFunction
      exitScope()

    case LambdaLiteral(args, expr) =>
      enterScope()
      args.foreach(v => declareVariable(v.value))
      val prevInFunction = inFunction
      inFunction = true
      analyzeExpression(expr)
      inFunction = prevInFunction
      exitScope()

    case FunctionCall(funcExpr, args) =>
      analyzeExpression(funcExpr)
      args.foreach(a => analyzeExpression(a))


    case ArrayAccess(target, idx) =>
      analyzeExpression(target)
      analyzeExpression(idx)

    case TupleFieldAccess(target, _) =>
      analyzeExpression(target)

    case TupleIndexAccess(target, _) =>
      analyzeExpression(target)

    case other =>
      throw new SemanticException(s"Unknown expression type: ${other.getClass.getSimpleName}")
  }

  // helpers
  private def enterScope(): Unit = scopes.push(mutable.Set.empty)

  private def exitScope(): Unit = if (scopes.nonEmpty) scopes.pop()

  private def declareVariable(name: String): Unit = scopes.head.add(name)

  private def isDeclared(name: String): Boolean = scopes.exists(_.contains(name))

  private def validateDeclaration(name: String): Unit = {
    if (!isDeclared(name)) {
      throw new SemanticException(s"Identifier '$name' not declared")
    }
  }
}
