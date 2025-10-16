package ru.innopolis.interpreter.syntax.analyzer.tree.statement.assignment

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.Statement

case class ArrayElementAssignment(target: Expression, index: Expression, value: Expression) extends Statement

