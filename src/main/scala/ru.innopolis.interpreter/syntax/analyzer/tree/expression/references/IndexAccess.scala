package ru.innopolis.interpreter.syntax.analyzer.tree.expression.references

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

case class IndexAccess(target: Expression, index:Int) extends Expression {

}
