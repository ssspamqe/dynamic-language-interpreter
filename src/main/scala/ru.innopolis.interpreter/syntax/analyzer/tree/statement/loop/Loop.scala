package ru.innopolis.interpreter.syntax.analyzer.tree.statement.loop

import ru.innopolis.interpreter.syntax.analyzer.tree.statement.{CodeBlock, Statement}

class Loop(val body: CodeBlock) extends Statement