package ru.innopolis.interpreter.syntax.analyzer.tree.expression

import ru.innopolis.interpreter.lexer.Code

case class Binary(operation:Code, left:Expression, right:Expression) extends Expression
