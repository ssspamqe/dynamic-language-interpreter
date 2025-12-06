package ru.innopolis.interpreter.runtime

import scala.collection.mutable

class Stack(private val outer: Option[Stack] = None) {

  private case class Frame(
                            variables: mutable.Map[String, Any] = mutable.Map.empty,
                          )

  private val frames: mutable.ListBuffer[Frame] = mutable.ListBuffer(Frame())

  def pushFrame(): Unit = {
    frames.prepend(Frame())
  }

  def popFrame(): Unit = {
    if (frames.nonEmpty && frames.size > 1) {
      frames.remove(0)
    }
  }

  def getVariable(name: String): Any = {
    frames.head.variables.get(name)
      .orElse(outer.flatMap(o => Option(o.getVariable(name))))
      .getOrElse(throw new RuntimeException(s"Variable '$name' is not defined in the top stack frame"))
  }

  def setVariable(name: String, value: Any): Unit = {
    frames.head.variables(name) = value
  }

  def defineVariable(name: String, value: Any): Unit = {
    frames.head.variables(name) = value
  }

  def hasVariable(name: String): Boolean = {
    frames.exists(_.variables.contains(name)) || outer.exists(_.hasVariable(name))
  }
}