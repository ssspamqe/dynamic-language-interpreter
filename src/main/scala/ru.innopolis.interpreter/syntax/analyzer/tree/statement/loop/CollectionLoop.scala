package ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.CodeBlock

case class CollectionLoop(ident: String,
                          collection: Expression,
                          override val body: CodeBlock
                         ) extends Loop(body)