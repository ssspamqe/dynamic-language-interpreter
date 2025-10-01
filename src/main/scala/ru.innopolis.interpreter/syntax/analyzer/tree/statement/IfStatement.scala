package ru.innopolis.interpreter.syntax.analyzer.tree.statement

import ru.innopolis.interpreter.syntax.analyzer.tree.expression.Expression

case class IfStatement(condition: Expression,
                       trueBranch: CodeBlock,
                       falseBranch: Option[CodeBlock]
                      ) extends Statement
