package ru.innopolis.interpreter.runtime

import scala.collection.mutable

/**
 * Runtime stack of scopes for storing variables and functions.
 * Each layer is a separate map; lookups go from top (innermost) to bottom (outermost).
 */
class Stack(private val outer: Option[Stack] = None) {

  private case class Layer(
    variables: mutable.Map[String, Any] = mutable.Map.empty,
    functions: mutable.Map[String, Any] = mutable.Map.empty
  )

  private val layers: mutable.ListBuffer[Layer] = mutable.ListBuffer(Layer())

  def addLayer(): Stack = {
    layers.prepend(Layer())
    this
  }

  def popLayer(): Unit = {
    if (layers.nonEmpty && layers.size > 1) {
      layers.remove(0)
    }
  }

  def getVariable(name: String): Any = {
    layers.iterator
      .flatMap(l => l.variables.get(name))
      .nextOption()
      .orElse(outer.map(_.getVariable(name)))
      .getOrElse(throw new RuntimeException(s"Variable '$name' is not defined"))
  }

  def setVariable(name: String, value: Any): Unit = {
    val inThisEnv = layers.reverseIterator.exists(_.variables.contains(name))
    if (inThisEnv) {
      layers.iterator.find(_.variables.contains(name)) match {
        case Some(layer) => layer.variables(name) = value
        case None => layers.head.variables(name) = value // fallback
      }
    } else {
      outer match {
        case Some(o) if o.hasVariable(name) => o.setVariable(name, value)
        case _ => layers.head.variables(name) = value
      }
    }
  }

  def defineVariable(name: String, value: Any): Unit = {
    layers.head.variables(name) = value
  }

  def hasVariable(name: String): Boolean = {
    layers.exists(_.variables.contains(name)) || outer.exists(_.hasVariable(name))
  }

  def getFunction(name: String): Any = {
    layers.iterator
      .flatMap(l => l.functions.get(name))
      .nextOption()
      .orElse(outer.map(_.getFunction(name)))
      .getOrElse(throw new RuntimeException(s"Function '$name' is not defined"))
  }

  def setFunction(name: String, value: Any): Unit = {
    layers.head.functions(name) = value
  }

  def defineFunction(name: String, value: Any): Unit = {
    layers.head.functions(name) = value
  }

  def hasFunction(name: String): Boolean = {
    layers.exists(_.functions.contains(name)) || outer.exists(_.hasFunction(name))
  }

  def createChild(): Stack = {
    new Stack(Some(this))
  }
}
