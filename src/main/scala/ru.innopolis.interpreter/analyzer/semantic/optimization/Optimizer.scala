package ru.innopolis.interpreter.analyzer.semantic.optimization

import ru.innopolis.interpreter.lexer._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.TypeCheck
import ru.innopolis.interpreter.syntax.analyzer.tree.statement._
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.assignment._
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop._

object Optimizer {

  def optimize(e: CodeBlock): CodeBlock = {
    val optimizedStatements = e.statements.foldLeft(List[Statement]())((ss, s) => s match {
      case e: ArrayElementAssignment => ss :+ ArrayElementAssignment(optimizeExpr(e.target), optimizeExpr(e.index), optimizeExpr(e.value))
      case e: VariableAssignment => ss :+ VariableAssignment(e.name, optimizeExpr(e.value))
      case e: VariableDeclaration =>
        val optimizedDecls = e.declarations.map { case (name, exprOpt) => (name, exprOpt.map(optimizeExpr)) }
        ss :+ VariableDeclaration(optimizedDecls)
      case e: CollectionLoop => ss :+ CollectionLoop(e.ident, optimizeExpr(e.collection), optimize(e.body))
      case e: RangeLoop => ss :+ RangeLoop(e.ident, optimizeExpr(e.from), optimizeExpr(e.to), optimize(e.body))
      case e: WhileLoop => ss :+ WhileLoop(optimizeExpr(e.condition), optimize(e.body))
      case e: Loop => ss :+ new Loop(optimize(e.body))
      case e: ExpressionStatement => ss :+ ExpressionStatement(optimizeExpr(e.expression))
      case e: IfStatement => ss :+ IfStatement(optimizeExpr(e.condition), optimize(e.trueBranch), e.falseBranch.map(optimize))
      case e: PrintStatement => ss :+ PrintStatement(e.expression.map(optimizeExpr))
      case e: ReturnStatement => ss :+ ReturnStatement(e.expression.map(optimizeExpr))
      case _ => ss :+ s
    })

    val usedVariables = collectUsedVariables(optimizedStatements)
    val filteredStatements: List[Statement] = optimizedStatements.flatMap {
      case VariableDeclaration(decls) =>
        val (used, unused) = decls.partition { case (name, _) => usedVariables.contains(name) }

        val keptDecls: List[Statement] =
          if (used.nonEmpty) List(VariableDeclaration(used))
          else Nil

        val sideEffectStmts: List[Statement] =
          unused.collect { case (_, Some(expr)) if hasSideEffect(expr) => ExpressionStatement(expr) }

        keptDecls ++ sideEffectStmts

      case other => List(other)
    }

    CodeBlock(filteredStatements)
  }

  private[optimization] def optimizeExpr(expr: Expression): Expression = expr match {
    case Binary(op, left, right) =>
      val l = optimizeExpr(left)
      val r = optimizeExpr(right)
      (l, r) match {
        case (Literal(a: Number), Literal(b: Number)) =>
          val ad = a.doubleValue()
          val bd = b.doubleValue()
          op match {
            case Code.PLUS =>
              foldNumeric(a, b, ad + bd)
            case Code.MINUS =>
              foldNumeric(a, b, ad - bd)
            case Code.MULTIPLICATION =>
              foldNumeric(a, b, ad * bd)
            case Code.DIVISION =>
              if (bd == 0) Binary(op, l, r)
              else {
                val isInt  = a.isInstanceOf[java.lang.Integer] && b.isInstanceOf[java.lang.Integer]
                val isLong = a.isInstanceOf[java.lang.Long]    && b.isInstanceOf[java.lang.Long]

                if (isInt)
                  Literal(Math.floor(ad / bd).toInt)
                else if (isLong)
                  Literal(Math.floor(ad / bd).toLong)
                else
                  Literal(ad / bd)
              }
            case Code.LESS => Literal(ad < bd)
            case Code.LESS_OR_EQUAL => Literal(ad <= bd)
            case Code.MORE => Literal(ad > bd)
            case Code.MORE_OR_EQUAL => Literal(ad >= bd)
            case Code.EQUAL => Literal(ad == bd)
            case Code.NOT_EQUAL => Literal(ad != bd)
          }
        case (Literal(a: String), Literal(b: String)) if op == Code.PLUS =>
          Literal(a + b)
        case (TupleLiteral(as), TupleLiteral(bs)) if op == Code.PLUS =>
          TupleLiteral(as ++ bs)
        case (ArrayLiteral(as), ArrayLiteral(bs)) if op == Code.PLUS =>
          ArrayLiteral(as ++ bs)
        case (Literal(a: Boolean), Literal(b: Boolean)) =>
          op match {
            case Code.AND => Literal(a && b)
            case Code.OR => Literal(a || b)
            case Code.XOR => Literal(a ^ b)
            case Code.EQUAL => Literal(a == b)
            case Code.NOT_EQUAL => Literal(a != b)
            case _ => Binary(op, l, r)
          }
        case _ => Binary(op, l, r)
      }
    case Unary(op, right) =>
      val r = optimizeExpr(right)
      r match {
        case Literal(a: Number) =>
          op match {
            case Code.MINUS =>
              a match {
                case i: java.lang.Integer => Literal(-i.intValue())
                case l: java.lang.Long => Literal(-l.longValue())
                case f: java.lang.Float => Literal(-f.floatValue())
                case d: java.lang.Double => Literal(-d.doubleValue())
              }
            case Code.PLUS =>
              Literal(a)
            case _ => Unary(op, r)
          }
        case Literal(a: Boolean) if op == Code.NOT =>
          Literal(!a)
        case _ =>
          Unary(op, r)
      }

    case ArrayAccess(t, i) =>
      ArrayAccess(optimizeExpr(t), optimizeExpr(i))

    case ArrayLiteral(xs) =>
      ArrayLiteral(xs.map(optimizeExpr))

    case TupleLiteral(es) =>
      TupleLiteral(es.map(e => e.copy(value = optimizeExpr(e.value))))

    case FunctionCall(target, args) =>
      FunctionCall(optimizeExpr(target), args.map(optimizeExpr))

    case _ => expr
  }

  private def isInt(d: Double): Boolean =
    d.isFinite && d % 1.0 == 0.0 && d >= Int.MinValue && d <= Int.MaxValue

  private def isLong(d: Double): Boolean =
    d.isFinite && d % 1.0 == 0.0 && d >= Long.MinValue && d <= Long.MaxValue

  private def foldNumeric(a: Number, b: Number, result: Double): Literal[_] = {
    if (a.isInstanceOf[Int] && b.isInstanceOf[Int] && isInt(result))
      Literal(result.toInt)
    else if (a.isInstanceOf[Long] && b.isInstanceOf[Long] && isLong(result))
      Literal(result.toLong)
    else
      Literal(result)
  }

  private[optimization] def collectUsedVariables(statements: List[Statement]): Set[String] = {
    def collectExpr(expr: Expression): Set[String] = expr match {
      case ArrayAccess(a, b) => collectExpr(a) ++ collectExpr(b)
      case ArrayAccess(a, b) => collectExpr(a) ++ collectExpr(b)
      case FunctionCall(a, b) => collectExpr(a) ++ b.flatMap(collectExpr(_))
      case TupleFieldAccess(a, _) => collectExpr(a)
      case TupleIndexAccess(a, _) => collectExpr(a)
      case Binary(_, l, r) => collectExpr(l) ++ collectExpr(r)
      case Unary(_, e) => collectExpr(e)
      case TypeCheck(inner, _) => collectExpr(inner)
      case LambdaLiteral(_, body) => collectExpr(body)
      case FunctionLiteral(_, body) => collectUsedVariables(body.statements)
      case ArrayLiteral(elements) => elements.flatMap(collectExpr).toSet
      case TupleLiteral(entries) => entries.flatMap(e => collectExpr(e.value)).toSet
      case Variable(name) => Set(name)
      case _ => Set.empty
    }

    statements.flatMap {
      case ExpressionStatement(expr) => collectExpr(expr)
      case VariableDeclaration(decls) =>
        decls.flatMap { case (_, exprOpt) => exprOpt.map(collectExpr).getOrElse(Set.empty) }
      case ArrayElementAssignment(t, i, v) => collectExpr(t) ++ collectExpr(i) ++ collectExpr(v)
      case VariableAssignment(name, exp) => Set(name) ++ collectExpr(exp)
      case CollectionLoop(_, c, body) => collectExpr(c) ++ collectUsedVariables(body.statements)
      case RangeLoop(_, f, t, body) => collectExpr(f) ++ collectExpr(t) ++ collectUsedVariables(body.statements)
      case WhileLoop(c, body) => collectExpr(c) ++ collectUsedVariables(body.statements)
      case Loop(body) => collectUsedVariables(body.statements)
      case IfStatement(c, t, f) => collectExpr(c) ++ collectUsedVariables(t.statements) ++ f.map(fb => collectUsedVariables(fb.statements)).getOrElse(Set.empty)
      case PrintStatement(e) => e.flatMap(collectExpr).toSet
      case _ => Set.empty
    }.toSet
  }

  private[optimization] def hasSideEffect(expr: Expression): Boolean = expr match {
    case FunctionCall(_, _) => true
    case Binary(_, l, r) => hasSideEffect(l) || hasSideEffect(r)
    case Unary(_, e) => hasSideEffect(e)
    case ArrayLiteral(elements) => elements.exists(hasSideEffect)
    case TupleLiteral(entries) => entries.exists(e => hasSideEffect(e.value))
    case _ => false
  }

}
