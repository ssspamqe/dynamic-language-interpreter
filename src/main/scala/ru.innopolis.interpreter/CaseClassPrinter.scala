package ru.innopolis.interpreter

object CaseClassPrinter {

  def printCaseClass(obj: Any, indent: String = ""): Unit = {
    obj match {
      case p: Product if p.productPrefix != "Tuple" =>
        val clsName = p.productPrefix
        val fields = p.getClass.getDeclaredFields.map(_.getName).zip(p.productIterator.toList)

        println(s"${clsName}(")
        for (((name, value), idx) <- fields.zipWithIndex) {
          val isLast = idx == fields.size - 1
          print(s"${indent} $name = ")
          value match {
            case inner: Product if inner.productPrefix != "Tuple" =>
              printCaseClass(inner, indent + "  ")
            case other =>
              println(other.toString)
          }
        }
        println(s"${indent})")
      case other =>
        println(s"${indent}${other.toString}")
    }
  }
}
