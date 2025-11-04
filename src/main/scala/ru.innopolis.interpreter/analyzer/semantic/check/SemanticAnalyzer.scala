package ru.innopolis.interpreter.syntax.analyzer.semantic

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

// Semantic exception for reporting errors
class SemanticException(msg: String) extends RuntimeException(msg)

// Small helper to represent known function signature (we only track parameter count here,
// because your AST doesn't carry static param types).
case class FuncSignature(paramCount: Int)

class SemanticAnalyzer {

  // stack of variable symbol tables (name -> TypeIndicator or FuncSignature encoded as special Type)
  private val scopes: mutable.Stack[mutable.Map[String, Either[TypeIndicator, FuncSignature]]] =
    mutable.Stack(mutable.Map.empty)

  // context flags while walking
  private var inFunction: Boolean = false
  private var inLoop: Boolean = false

  // entry point
  def analyze(block: CodeBlock): Unit = {
    analyzeCodeBlock(block)
  }

  // analyze statements and blocks
  private def analyzeCodeBlock(block: CodeBlock): Unit = {
    // new nested lexical scope for each code block
    enterScope()
    block.statements.foreach(analyzeStatement)
    exitScope()
  }

  private def analyzeStatement(stmt: Statement): Unit = stmt match {
    case VariableDeclaration(name, expr) =>
      val t = analyzeExpression(expr)
      // if expression is a function literal, we register a function signature instead of plain type
      expr match {
        case FunctionLiteral(args, _) =>
          declareFunctionValue(name, FuncSignature(args.length))
        case LambdaLiteral(args, _) =>
          declareFunctionValue(name, FuncSignature(args.length))
        case _ =>
          declareVariable(name, t)
      }

    case VariableAssignment(name, expr) =>
      val valueType = analyzeExpression(expr)
      val sym = lookup(name)
      sym match {
        case Left(declaredType) =>
          if (!assignable(declaredType, valueType))
            throw new SemanticException(s"Type mismatch on assignment to '$name': expected $declaredType but got $valueType")
        case Right(_) =>
          throw new SemanticException(s"Cannot assign non-function value to function identifier '$name'")
      }

    case ArrayElementAssignment(target, index, expr) =>
      val targetType = analyzeExpression(target)
      val indexType = analyzeExpression(index)
      if (indexType != TypeIndicator.IntType)
        throw new SemanticException(s"Array index must be Int, got $indexType")
      if (targetType != TypeIndicator.ArrayType)
        throw new SemanticException(s"Target of array assignment is not array, got $targetType")
      // we don't track element types in TypeIndicator, so just check value is a valid array element type:
      val valueType = analyzeExpression(expr)
      // allow any type as array element (since element type isn't tracked). If you want stronger checks, extend TypeIndicator.
      ()

    case ExpressionStatement(expr) =>
      analyzeExpression(expr)

    case ReturnStatement(maybeExpr) =>
      if (!inFunction) throw new SemanticException("`return` may be used only inside a function")
      maybeExpr.foreach(analyzeExpression)

    case PrintStatement(exprs) =>
      exprs.foreach(analyzeExpression)

    case IfStatement(cond, thenBlock, elseBlock) =>
      val condType = analyzeExpression(cond)
      if (condType != TypeIndicator.BoolType)
        throw new SemanticException(s"If condition must be Bool, got $condType")
      // then/else are nested scopes
      analyzeCodeBlock(thenBlock)
      elseBlock.foreach(analyzeCodeBlock)

    case WhileLoop(cond, body) =>
      val condType = analyzeExpression(cond)
      if (condType != TypeIndicator.BoolType)
        throw new SemanticException(s"While condition must be Bool, got $condType")
      val prevInLoop = inLoop
      inLoop = true
      try analyzeCodeBlock(body) finally inLoop = prevInLoop

    case Loop(body) =>
      val prevInLoop = inLoop
      inLoop = true
      try analyzeCodeBlock(body) finally inLoop = prevInLoop

    case CollectionLoop(varName, collectionExpr, body) =>
      val collType = analyzeExpression(collectionExpr)
      // we don't track element types of collections precisely; but collection must be Array or Tuple.
      if (!(collType == TypeIndicator.ArrayType || collType == TypeIndicator.TupleType))
        throw new SemanticException(s"Collection in loop must be an array or tuple, got $collType")
      // declare loop variable as unknown element type — we set NoneType as placeholder
      enterScope()
      try {
        declareVariable(varName, TypeIndicator.NoneType)
        val prevInLoop = inLoop
        inLoop = true
        try analyzeCodeBlock(body) finally inLoop = prevInLoop
      } finally exitScope()

    case RangeLoop(optVar, from, to, body) =>
      val fromT = analyzeExpression(from)
      val toT = analyzeExpression(to)
      if (!(fromT == TypeIndicator.IntType || fromT == TypeIndicator.RealType))
        throw new SemanticException(s"Range start must be numeric, got $fromT")
      if (!(toT == TypeIndicator.IntType || toT == TypeIndicator.RealType))
        throw new SemanticException(s"Range end must be numeric, got $toT")
      optVar.foreach { v =>
        enterScope()
        try {
          declareVariable(v, TypeIndicator.IntType)
          val prevInLoop = inLoop
          inLoop = true
          try analyzeCodeBlock(body) finally inLoop = prevInLoop
        } finally exitScope()
      }
      if (optVar.isEmpty) {
        val prevInLoop = inLoop
        inLoop = true
        try analyzeCodeBlock(body) finally inLoop = prevInLoop
      }

    case ExitStatement() =>
      if (!inLoop) throw new SemanticException("`exit` (break) may be used only inside a loop")

    case other =>
      // fallback: try to analyze contained expressions if any
      other match {
        case s: Statement => // nothing
        case _ => ()
      }
  }

  private def enterScope(): Unit = scopes.push(mutable.Map.empty)

  private def exitScope(): Unit = {
    if (scopes.nonEmpty) scopes.pop()
  }

  private def declareVariable(name: String, t: TypeIndicator): Unit = {
    val current = scopes.head
    if (current.contains(name)) throw new SemanticException(s"Variable '$name' already declared in this scope")
    current(name) = Left(t)
  }

  private def declareFunctionValue(name: String, sig: FuncSignature): Unit = {
    val current = scopes.head
    if (current.contains(name)) throw new SemanticException(s"Identifier '$name' already declared in this scope")
    current(name) = Right(sig)
  }

  private def lookup(name: String): Either[TypeIndicator, FuncSignature] = {
    scopes.find(_.contains(name)) match {
      case Some(scope) => scope(name)
      case None => throw new SemanticException(s"Identifier '$name' not declared")
    }
  }

  // compatibility for assignment: exact match OR int -> real widening allowed
  private def assignable(dest: TypeIndicator, value: TypeIndicator): Boolean = {
    if (dest == value) true
    else (dest, value) match {
      case (TypeIndicator.RealType, TypeIndicator.IntType) => true
      case _ => false
    }
  }


  // expression analysis & type inference
  private def analyzeExpression(expr: Expression): TypeIndicator = expr match {
    case Literal(v) =>
      v match {
        case _: Int => TypeIndicator.IntType
        case _: Long => TypeIndicator.IntType
        case _: Double => TypeIndicator.RealType
        case _: String => TypeIndicator.StringType
        case _: Boolean => TypeIndicator.BoolType
        case None => TypeIndicator.NoneType
        case other => throw new SemanticException(s"Unknown literal type: $other")
      }

    case Variable(name) =>
      lookup(name) match {
        case Left(t) => t
        case Right(_) => TypeIndicator.FuncType // using FuncType flag when identifier denotes function value
      }

    case Unary(op, inner) =>
      val t = analyzeExpression(inner)
      op match {
        case Code.MINUS =>
          if (t == TypeIndicator.IntType) t
          else if (t == TypeIndicator.RealType) t
          else throw new SemanticException(s"Unary ${op} requires numeric operand (Int or Real), got $t")
        case Code.NOT =>
          if (t == TypeIndicator.BoolType) TypeIndicator.BoolType
          else throw new SemanticException(s"`not` requires Bool operand, got $t")
        case _ =>
          throw new SemanticException(s"Unknown unary operator: $op")
      }

    case Binary(op, left, right) =>
      val lt = analyzeExpression(left)
      val rt = analyzeExpression(right)

      op match {
        // arithmetic +, -, *, /
        case Code.PLUS | Code.MINUS | Code.MULTIPLICATION | Code.DIVISION =>
          // special-case concatenation: string+string, tuple+tuple, array+array
          if (op == Code.PLUS) {
            if (lt == TypeIndicator.StringType && rt == TypeIndicator.StringType) return TypeIndicator.StringType
            if (lt == TypeIndicator.TupleType && rt == TypeIndicator.TupleType) return TypeIndicator.TupleType
            if (lt == TypeIndicator.ArrayType && rt == TypeIndicator.ArrayType) return TypeIndicator.ArrayType
          }
          // numeric arithmetic
          if (allNumerics(lt, rt)) {
            // promotion rules: Int op Int -> Int (except division specified)
            if (op == Code.DIVISION) {
              (lt, rt) match {
                case (TypeIndicator.IntType, TypeIndicator.IntType) => TypeIndicator.IntType // rounding down as semantics
                case (TypeIndicator.IntType, TypeIndicator.RealType) => TypeIndicator.RealType
                case (TypeIndicator.RealType, TypeIndicator.IntType) => TypeIndicator.RealType
                case (TypeIndicator.RealType, TypeIndicator.RealType) => TypeIndicator.RealType
              }
            } else {
              if (lt == TypeIndicator.RealType || rt == TypeIndicator.RealType) TypeIndicator.RealType
              else TypeIndicator.IntType
            }
          } else {
            throw new SemanticException(s"Operator ${op} not applicable to types $lt and $rt")
          }

        // comparisons: <, >, <=, >=, =, /=
        case Code.LESS | Code.MORE | Code.LESS_OR_EQUAL | Code.MORE_OR_EQUAL | Code.EQUAL | Code.NOT_EQUAL =>
          if (allNumerics(lt, rt)) {
            TypeIndicator.BoolType
          } else {
            throw new SemanticException(s"Comparison ${op} requires numeric operands (Int/Real), got $lt and $rt")
          }

        // logical: and, or, xor
        case Code.AND | Code.OR | Code.XOR =>
          if (lt == TypeIndicator.BoolType && rt == TypeIndicator.BoolType) TypeIndicator.BoolType
          else throw new SemanticException(s"Logical operator requires Bool operands, got $lt and $rt")

        case _ => throw new SemanticException(s"Unknown binary operator: $op")
      }

    case TypeCheck(inner, _) =>
      analyzeExpression(inner)
      TypeIndicator.BoolType

    case ArrayLiteral(elements) =>
      elements.foreach(analyzeExpression)
      TypeIndicator.ArrayType

    case TupleLiteral(elems) =>
      elems.foreach { case TupleEntry(_, e) => analyzeExpression(e) }
      TypeIndicator.TupleType

    case FunctionLiteral(args, body) =>
      // when evaluating a function literal, we create a signature and analyze body under function context
      val sig = FuncSignature(args.length)

      // analyze body: new scope with parameters declared
      enterScope()
      try {
        // declare parameters as NoneType placeholders (no static param types in grammar)
        args.foreach(v => declareVariable(v.value, TypeIndicator.NoneType))
        val prevInFunction = inFunction
        val prevInLoop = inLoop
        inFunction = true
        inLoop = false
        try {
          analyzeCodeBlock(body)
        } finally {
          inFunction = prevInFunction
          inLoop = prevInLoop
        }
      } finally exitScope()
      TypeIndicator.FuncType

    case LambdaLiteral(args, expr) =>
      // lambda: similar to function literal but single expression body
      enterScope()
      try {
        args.foreach(v => declareVariable(v.value, TypeIndicator.NoneType))
        val prevInFunction = inFunction
        val prevInLoop = inLoop
        inFunction = true
        inLoop = false
        try {
          analyzeExpression(expr)
        } finally {
          inFunction = prevInFunction
          inLoop = prevInLoop
        }
      } finally exitScope()
      TypeIndicator.FuncType

    case FunctionCall(funcExpr, args) =>
      // callee must be a function value
      val funcType = funcExpr match {
        case Variable(name) =>
          lookup(name) match {
            case Right(sig) =>
              // check arg count
              if (sig.paramCount != args.length)
                throw new SemanticException(s"Function '$name' expects ${sig.paramCount} arguments but got ${args.length}")
            case Left(t) =>
              // the identifier is not a function
              throw new SemanticException(s"Identifier '$name' is not a function (type: $t)")
          }
        case other =>
          // evaluate callee expression — it could be an inline function literal
          val ct = analyzeExpression(other)
          // if it's a function literal used directly, there's no stored signature. We can check if the expression is a function literal AST node:
          other match {
            case FunctionLiteral(params, _) =>
              if (params.length != args.length)
                throw new SemanticException(s"Function literal expects ${params.length} arguments but got ${args.length}")
            case LambdaLiteral(params, _) =>
              if (params.length != args.length)
                throw new SemanticException(s"Lambda expects ${params.length} arguments but got ${args.length}")
            case _ =>
              // if callee evaluated to FuncType flag but we don't have signature, we can't check arg count => error
              if (ct == TypeIndicator.FuncType)
                throw new SemanticException("Cannot determine function parameter count for this call")
              else
                throw new SemanticException(s"Attempt to call non-function expression (type: $ct)")
          }
      }
      // evaluate all args
      args.foreach(analyzeExpression)
      // we don't track real return types for functions — use NoneType as placeholder
      TypeIndicator.NoneType

    case ArrayAccess(target, idx) =>
      val t = analyzeExpression(target)
      val it = analyzeExpression(idx)
      if (t != TypeIndicator.ArrayType) throw new SemanticException(s"Attempt to index non-array type: $t")
      if (it != TypeIndicator.IntType) throw new SemanticException(s"Array index must be Int, got $it")
      // element type unknown -> use NoneType placeholder
      TypeIndicator.NoneType

    case TupleFieldAccess(target, _) =>
      val tt = analyzeExpression(target)
      if (tt != TypeIndicator.TupleType) throw new SemanticException(s"Tuple field access on non-tuple type: $tt")
      TypeIndicator.NoneType

    case TupleIndexAccess(target, _) =>
      val tt = analyzeExpression(target)
      if (tt != TypeIndicator.TupleType) throw new SemanticException(s"Tuple index access on non-tuple type: $tt")
      TypeIndicator.NoneType

    case other =>
      throw new SemanticException(s"Unhandled expression in semantic analysis: $other")
  }

  private def allNumerics(t: TypeIndicator*): Boolean = {
    t.forall(isNumeric)
  }

  private def isNumeric(t: TypeIndicator): Boolean = {
    t == TypeIndicator.IntType || t == TypeIndicator.RealType
  }
}
