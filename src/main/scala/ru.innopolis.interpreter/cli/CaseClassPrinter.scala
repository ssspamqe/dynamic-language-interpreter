package ru.innopolis.interpreter.cli

object CaseClassPrinter {

  private val branch = "├── "
  private val lastBranch = "└── "
  private val vertical = "│   "
  private val emptyPad = "    "

  def printCaseClass(obj: Any,
                     prefix: String = "",
                     isLast: Boolean = true): Unit = {

    val connector =
      if (isLast) lastBranch else branch

    obj match {

      // ---------------- Iterable ----------------
      case it: Iterable[_] =>
        //        println(prefix + connector + it.getClass.getSimpleName)
        val newPrefix =
          prefix + (if (isLast) emptyPad else vertical)

        val seq = it.toSeq
        seq.zipWithIndex.foreach { case (elem, idx) =>
          val last = idx == seq.size - 1
          printCaseClass(elem, newPrefix, last)
        }

      // ---------------- Case class ----------------
      case p: Product if !p.productPrefix.startsWith("Tuple") =>
        println(prefix + connector + p.productPrefix)

        val names =
          p.getClass.getDeclaredFields.map(_.getName).toList

        val values = p.productIterator.toList
        val fields = names.zip(values)

        val newPrefix =
          prefix + (if (isLast) emptyPad else vertical)

        fields.zipWithIndex.foreach { case ((name, value), idx) =>
          val lastField = idx == fields.size - 1

          // Печатаем имя поля
          val fieldPrefix =
            newPrefix + (if (lastField) lastBranch else branch)

          value match {
            case inner: Product
              if !inner.productPrefix.startsWith("Tuple") =>
              println(fieldPrefix + name)
              printCaseClass(
                inner,
                newPrefix + (if (lastField) emptyPad else vertical),
                isLast = true
              )

            case it: Iterable[_] =>
              println(fieldPrefix + name)
              printCaseClass(
                it,
                newPrefix + (if (lastField) emptyPad else vertical),
                isLast = true
              )

            case other =>
              println(fieldPrefix + s"$name = $other")
          }
        }

      // ---------------- Primitive / String ----------------
      case other =>
        println(prefix + connector + other.toString)
    }
  }
}

object test extends App {
  CaseClassPrinter.printCaseClass((1, 2, 3))
}