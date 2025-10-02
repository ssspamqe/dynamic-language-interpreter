package ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

case class VariableDeclaration(name: String, expression: Expression) extends Declaration(name)
