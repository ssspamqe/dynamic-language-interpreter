package ru.innopolis.interpreter.syntax.analyzer.tree.statement.assignment

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.Statement

case class VariableAssignment(name: String, value: Expression) extends Statement

