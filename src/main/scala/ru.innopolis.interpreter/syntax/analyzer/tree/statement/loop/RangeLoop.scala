package ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression
import ru.innopolis.interpreter.syntax.analyzer.tree.statement.CodeBlock

case class RangeLoop(ident: Option[String],
                     from:Expression,
                     to:Expression,
                     body:CodeBlock
                  ) extends ForLoop(ident, body)