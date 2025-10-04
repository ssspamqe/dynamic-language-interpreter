package ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.CodeBlock

case class CollectionLoop(ident: Option[String],
                          collection: Expression,
                          body: CodeBlock
                         ) extends ForLoop(ident, body)