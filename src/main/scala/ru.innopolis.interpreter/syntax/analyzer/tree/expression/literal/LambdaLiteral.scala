package ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.{Expression, Variable}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.ExpressionStatement

case class LambdaLiteral(args: List[Variable], body: Expression) extends Expression

