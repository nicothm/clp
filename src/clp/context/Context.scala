package clp.context

import clp.converters._

/**
 * The context holds all parsed arguments as List[String].
 * To get a value as another type you need an ListStringConverter in the scope.
 * For the most-used datatypes they are defined and can be imported from [[clp.converters.Convertable._
 */
trait Context {
  def get[T : ListStringConverter](key: String): Option[T]
  def as[T : ListStringConverter](key: String): T
  def isDefined(key: String): Boolean
}

class DefaultContext(private val map:Map[String, List[String]]) extends Context {
  override def get[T: ListStringConverter](key: String): Option[T] = map.get(key).map { xs =>
    implicitly[ListStringConverter[T]].convert(xs)
  }

  override def isDefined(key: String): Boolean = map.isDefinedAt(key)

  override def as[T: ListStringConverter](key: String): T = get(key).get
  override def toString:String = "Context: \n"+map.toString
}
