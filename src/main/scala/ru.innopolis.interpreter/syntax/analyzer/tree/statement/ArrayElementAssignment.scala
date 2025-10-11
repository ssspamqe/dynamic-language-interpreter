package ru.innopolis.interpreter.syntax.analyzer.tree.statement

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

case class ArrayElementAssignment(target: Expression, index: Expression, value: Expression) extends Statement

