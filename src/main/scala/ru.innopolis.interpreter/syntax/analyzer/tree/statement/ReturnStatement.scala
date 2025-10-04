package ru.innopolis.interpreter.syntax.analyzer.tree.statement

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

case class ReturnStatement(expression: Option[Expression]) extends Statement