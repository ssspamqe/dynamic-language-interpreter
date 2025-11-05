package ru.innopolis.interpreter.analyzer.semantic.optimization

import ru.innopolis.interpreter.lexer._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal._
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.references._
import ru.innopolis.interpreter.syntax.analyzer.tree.statement._
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration.VariableDeclaration

/*
Possible Optimizations
Here are some common optimizations you might want to implement. These optimizations modify the AST:
1. Constant Expression Simplification: Simplify constant expressions during compilation. For example, a = 5 + 3 can be reduced to a = 8, and expressions like 3 < 5 can be replaced with True.
2. Removing Unused Variables: If variables are declared but never used in the program, they can be safely removed to reduce code clutter.
3. Function Inlining: Replace function calls with the function body itself to reduce the overhead of function calls.
Example: Before inlining:
  func foo(a : Integer) {
    print("Hello")
    print(a)
  }

  func main() {
    a : Integer = 1
    foo(a)

    foo(2)
  }
After inlining:
  func main() {
    a : Integer = 1
    print("Hello") // inlined foo(a)
    print(a)

    print("Hello") // inlined foo(2)
    print(2)
  }
4. Code simplification: Simplify conditional structures where possible.
  Example: Before:
  if (True) {
    print("Hello")
    print("There")
  }
  else {
    print("Otherwise")
  }
  After simplification:
  print("Hello") // if branch is always true. Completely remove else branch and replace the whole if structure with its body.
  print("There")
5. Removing Unreachable Code: Remove any code that will never be executed. For instance, code after a return statement:
  func foo() {
    return 0
    print("Hello") // this is unreachable code and can be removed at compile time
  }
 */

object Optimizer {

  def optimize(e: CodeBlock): CodeBlock = {
    // evaluate constants
    val optimizedStatements = e.statements.foldLeft(List[Statement]())((ss, s) => s match {
      case e: ExpressionStatement => ss :+ ExpressionStatement(optimizeExpr(e.expression))
      case e: VariableDeclaration => ss :+ VariableDeclaration(e.name, optimizeExpr(e.expression))
      case _ => ss :+ s
    })

    // remove unused variables
    val usedVariables = collectUsedVariables(optimizedStatements)
    val filteredStatements = optimizedStatements.flatMap {
      case vd@VariableDeclaration(name, expr) =>
        if (usedVariables.contains(name)) Some(vd)
        else {
          if (hasSideEffect(expr)) Some(ExpressionStatement(expr))
          else None
        }
      case other => Some(other)
    }

    CodeBlock(filteredStatements)
  }

  private def optimizeExpr(expr: Expression): Expression = expr match {
    case Binary(op, left, right) =>
      (optimizeExpr(left), optimizeExpr(right)) match {
        case (Literal(an: Number), Literal(bn: Number)) =>
          val a = an.doubleValue()
          val b = bn.doubleValue()
          op match {
            case Code.PLUS =>
              Literal(a + b)
            case Code.MINUS =>
              Literal(a - b)
            case Code.MULTIPLICATION =>
              Literal(a * b)
            case Code.DIVISION if b != 0 && b != 0.0 =>
              Literal(a / b)
            case Code.DIVISION =>
              Binary(op, Literal(a), Literal(b))
            case Code.LESS => Literal(a < b)
            case Code.LESS_OR_EQUAL => Literal(a <= b)
            case Code.MORE => Literal(a > b)
            case Code.MORE_OR_EQUAL => Literal(a >= b)
            case Code.EQUAL => Literal(a == b)
            case Code.NOT_EQUAL => Literal(a != b)
            case _ => Binary(op, Literal(a), Literal(b))
          }
        case (Literal(a: Boolean), Literal(b: Boolean)) => op match {
          case Code.AND => Literal(a && b)
          case Code.OR => Literal(a || b)
          case Code.XOR => Literal(a ^ b)
          case Code.EQUAL => Literal(a == b)
          case Code.NOT_EQUAL => Literal(a != b)
          case _ => Binary(op, Literal(a), Literal(b))
        }
        case (l, r) => Binary(op, l, r)
      }

    case Unary(op, right) =>
      optimizeExpr(right) match {
        case Literal(a: Number) => op match {
          case Code.MINUS =>
            a match {
              case ai: java.lang.Integer => Literal(-ai.intValue())
              case al: java.lang.Long => Literal(-al.longValue())
              case ad: java.lang.Double => Literal(-ad.doubleValue())
              case af: java.lang.Float => Literal(-af.floatValue())
              case _ => Unary(op, Literal(a))
            }

          case _ => Unary(op, Literal(a))
        }
        case Literal(a: Boolean) => op match {
          case Code.NOT => Literal(!a)
          case _ => Unary(op, Literal(a))
        }
        case r => Unary(op, r)
      }

    case ArrayLiteral(elements) =>
      ArrayLiteral(elements.map(optimizeExpr))
    case TupleLiteral(entries) =>
      TupleLiteral(entries.map(te => te.copy(value = optimizeExpr(te.value))))
    case FunctionCall(target, args) =>
      FunctionCall(optimizeExpr(target), args.map(optimizeExpr))
    case _ => expr
  }

  private def collectUsedVariables(statements: List[Statement]): Set[String] = {
    def collectExpr(expr: Expression): Set[String] = expr match {
      case ArrayAccess(Variable(name), _) => Set(name)
      case FunctionCall(Variable(name), _) => Set(name)
      case TupleFieldAccess(Variable(name), _) => Set(name)
      case TupleIndexAccess(Variable(name), _) => Set(name)
      case Binary(_, l, r) => collectExpr(l) ++ collectExpr(r)
      case Unary(_, e) => collectExpr(e)
      case FunctionCall(target, args) =>
        collectExpr(target) ++ args.flatMap(collectExpr).toSet
      case ArrayLiteral(elements) => elements.flatMap(collectExpr).toSet
      case TupleLiteral(entries) => entries.flatMap(e => collectExpr(e.value)).toSet
      case Variable(name) => Set(name)
      case _ => Set.empty
    }

    statements.flatMap {
      case ExpressionStatement(expr) => collectExpr(expr)
      case VariableDeclaration(_, expr) => collectExpr(expr)
      case _ => Set.empty
    }.toSet
  }

  private def hasSideEffect(expr: Expression): Boolean = expr match {
    case FunctionCall(_, _) => true
    case Binary(_, l, r) => hasSideEffect(l) || hasSideEffect(r)
    case Unary(_, e) => hasSideEffect(e)
    case ArrayLiteral(elements) => elements.exists(hasSideEffect)
    case TupleLiteral(entries) => entries.exists(e => hasSideEffect(e.value))
    case _ => false
  }

}
