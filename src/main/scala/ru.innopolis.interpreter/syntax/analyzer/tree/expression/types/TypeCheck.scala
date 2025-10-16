package ru.innopolis.interpreter.syntax.analyzer.tree.expression.types

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression
import ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.indicator.TypeIndicator

case class TypeCheck(expression: Expression, typeIndicator: TypeIndicator) extends Expression

