package ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop

import ru.innopolis.interpreter.syntax.analyzer.tree.statement.CodeBlock

class ForLoop(ident:Option[String], override val body:CodeBlock) extends Loop(body) {

}
