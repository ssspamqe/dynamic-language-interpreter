package ru.innopolis.interpreter.syntax.analyzer.tree.expression.literal

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

case class Literal[T](value: T) extends Expression
