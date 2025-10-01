package ru.innopolis.interpreter.syntax.analyzer.tree.expression

import ru.innopolis.interpreter.lexer.Code

case class Unary(operation: Code, right:Expression) extends Expression