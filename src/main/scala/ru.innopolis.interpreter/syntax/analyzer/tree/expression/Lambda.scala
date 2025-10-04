package ru.innopolis.interpreter.syntax.analyzer.tree.expression

import ru.innopolis.interpreter.syntax.analyzer.tree.statement.CodeBlock

case class Lambda(params: List[String], body:Expression) extends Expression
