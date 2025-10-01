package ru.innopolis.interpreter.syntax.analyzer.tree.expression.references

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

case class FieldAccess(target: Expression, field: String) extends Expression {

}
