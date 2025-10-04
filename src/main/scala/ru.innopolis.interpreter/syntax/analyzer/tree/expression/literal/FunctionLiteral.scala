package ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.{Expression, Variable}
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.CodeBlock

case class FunctionLiteral(args: List[Variable], body: Either[Expression, CodeBlock]) extends Expression
