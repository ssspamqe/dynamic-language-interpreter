package ru.innopolis.interpreter.syntax.analyzer.tree.expression.references

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

case class ArrayAccess(target: Expression, index: Expression) extends Expression
