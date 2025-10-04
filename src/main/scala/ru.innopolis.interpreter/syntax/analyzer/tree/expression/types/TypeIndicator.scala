package ru.innopolis.interpreter.syntax.analyzer.tree.expression.types.indicator

sealed trait TypeIndicator

object TypeIndicator {
  case object IntType extends TypeIndicator
  case object RealType extends TypeIndicator
  case object BoolType extends TypeIndicator
  case object StringType extends TypeIndicator
  case object NoneType extends TypeIndicator
  case object ArrayType extends TypeIndicator
  case object TupleType extends TypeIndicator
  case object FuncType extends TypeIndicator
}