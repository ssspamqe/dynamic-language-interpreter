package ru.innopolis.interpreter.syntax.analyzer.tree.statement.declaration

import ru.innopolis.interpreter.syntax.analyzer.tree.statement.CodeBlock

case class FunctionDeclaration(name: String, body:CodeBlock) extends Declaration(name)