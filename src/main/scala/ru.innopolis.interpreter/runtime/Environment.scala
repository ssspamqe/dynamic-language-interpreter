package ru.innopolis.interpreter.runtime

import scala.collection.mutable

/**
 * Runtime environment for storing variables and functions
 */
class Environment(parent: Option[Environment] = None) {
  private val variables = mutable.Map[String, Any]()
  private val functions = mutable.Map[String, Any]()

  def getVariable(name: String): Any = {
    variables.get(name) match {
      case Some(value) => value
      case None => parent match {
        case Some(p) => p.getVariable(name)
        case None => throw new RuntimeException(s"Variable '$name' is not defined")
      }
    }
  }

  def setVariable(name: String, value: Any): Unit = {
    if (variables.contains(name) || parent.isEmpty || !parent.get.hasVariable(name)) {
      variables(name) = value
    } else {
      parent.get.setVariable(name, value)
    }
  }

  def defineVariable(name: String, value: Any): Unit = {
    variables(name) = value
  }

  def hasVariable(name: String): Boolean = {
    variables.contains(name) || parent.exists(_.hasVariable(name))
  }

  def getFunction(name: String): Any = {
    functions.get(name) match {
      case Some(value) => value
      case None => parent match {
        case Some(p) => p.getFunction(name)
        case None => throw new RuntimeException(s"Function '$name' is not defined")
      }
    }
  }

  def setFunction(name: String, value: Any): Unit = {
    functions(name) = value
  }

  def hasFunction(name: String): Boolean = {
    functions.contains(name) || parent.exists(_.hasFunction(name))
  }

  def createChild(): Environment = {
    new Environment(Some(this))
  }
}

