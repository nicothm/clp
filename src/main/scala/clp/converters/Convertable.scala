package clp.converters

trait Convertable[A, B] {
  def convert(elem: B) : A
}

trait ListStringConverter[A] extends Convertable[A, List[String]]
trait StringConverter[A] extends Convertable[A, String]

object Convertable {
  implicit object intConverter extends StringConverter[Int] {
    override def convert(elem: String): Int = elem.toInt
  }
  implicit object doubleConverter extends StringConverter[Double] {
    override def convert(elem: String): Double = elem.toDouble
  }
  implicit object booleanConverter extends StringConverter[Boolean] {
    override def convert(elem: String): Boolean = elem == "true"
  }
  implicit object stringConverter extends StringConverter[String] {
    override def convert(elem: String): String = elem
  }

  def convertVal[T : StringConverter]: ListStringConverter[T] = new ListStringConverter[T] {
    override def convert(elem: List[String]): T = implicitly[StringConverter[T]].convert(elem.head)
  }

  def convertList[T : StringConverter]: ListStringConverter[List[T]] = new ListStringConverter[List[T]] {
    override def convert(elem: List[String]): List[T] = elem.map(implicitly[StringConverter[T]].convert)
  }

  //converters for values
  implicit val stringToInt: ListStringConverter[Int] = convertVal[Int]
  implicit val stringToDouble: ListStringConverter[Double] = convertVal[Double]
  implicit val stringToString: ListStringConverter[String] = convertVal[String]
  implicit val stringToBoolean: ListStringConverter[Boolean] = convertVal[Boolean]

  //converters for lists
  implicit val intList = convertList[Int]
  implicit val doubleList = convertList[String]
  implicit val stringList = convertList[Double]
}

