package clp.config
import clp.types._

/**
 * Configs are used to define a command-line argument.
 * Each config has a name (flag), a type and a description.
 */
sealed trait Config {
  def getNames(): List[String]
  def description: String
  def namesContains(name: String): Boolean
  def getType():ParseTy
  def isRequired(): Boolean

  def alias(name:String): Config = DefaultConfig(name :: this.getNames(), this.getType(), description, isRequired())
  def <<(descr:String): Config = DefaultConfig(this.getNames(), this.getType(), descr, isRequired())
  def required: Config = DefaultConfig(getNames(), getType(), description, true)
}

case class DefaultConfig(names: List[String], ty :ParseTy, descr:String = "", isRequired:Boolean=false) extends Config {
  def getNames(): List[String] = names
  def description: String = descr
  def namesContains(name: String): Boolean = names.contains(name)
  def getType() = ty
}

object Config {
  def name(names: String*): Config = DefaultConfig(names.toList, DefaultType)
  def apply(name:String, ty:ParseTy) = DefaultConfig(List(name), ty)
}