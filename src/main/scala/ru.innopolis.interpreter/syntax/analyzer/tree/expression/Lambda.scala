package ru.innopolis.interpreter.syntax.analyzer.tree.expression

case class Lambda(params: List[String], body:Expression) extends Expression
