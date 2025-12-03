package ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.Statement

case class VariableDeclaration(declarations: List[(String, Expression)]) extends Statement
