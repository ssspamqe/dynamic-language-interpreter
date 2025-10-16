package ru.innopolis.interpreter.syntax.analyzer.tree.statement

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

case class ExpressionStatement(expression:Expression) extends Statement {

}
