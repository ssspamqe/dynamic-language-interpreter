package ru.innopolis.interpreter.syntax.analyzer.tree.statement

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

case class PrintStatement(expression: List[Expression]) extends Statement