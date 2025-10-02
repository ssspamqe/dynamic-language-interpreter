package ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.CodeBlock

case class WhileLoop(condition: Expression, body: CodeBlock) extends Loop(body)