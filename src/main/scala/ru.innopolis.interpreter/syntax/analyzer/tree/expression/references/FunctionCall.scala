package ru.innopolis.interpreter.syntax.analyzer.tree.expression.references

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

case class FunctionCall(target: Expression, args: List[Expression]) extends Expression

