package clp.types

/**
 * Each parse-type validates that the given string can be parsed into a specific type.
 */
trait ParseTy {
  def validate(v:Seq[String]):Boolean = v.forall(validate)
  def validate(v: String): Boolean
}

case object StringTy extends ParseTy {
  override def validate(v: String): Boolean = true
}

case object IntTy extends ParseTy {
  override def validate(v: String): Boolean = v.forall(_.isDigit)
}

case object DoubleTy extends ParseTy {
  override def validate(v: String): Boolean = try {
    v.toDouble
    true
  } catch {
    case _:NumberFormatException => false
  }
}

case object FlagTy extends ParseTy {
  override def validate(v: String): Boolean = true
}

case object BooleanTy extends ParseTy {
  override def validate(v: String): Boolean = v == "true" || v == "false"
}

case class ListTy(base:ParseTy) extends ParseTy {
  override def validate(v: String): Boolean = base.validate(v)
}