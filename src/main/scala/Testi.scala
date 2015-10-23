import clp.config.Config
import clp._
import clp.converters.{ListStringConverter, StringConverter}
import clp.types._
import clp.Parser._

object Testi {

  case class Real(d:Double)

  case object RealTy extends ParseTy {
    override def validate(v: String): Boolean = v.split("\\.")(1).length <= 2
  }

  def main(args: Array[String]) {
    val configs = Array[Config](
      "--heapsize" -> Int << "Specify heapsize in bytes",
      ("--pm" -> RealTy).alias("-p").alias("--superP"),
      "--pc" -> Int,
      "-f" -> Boolean << "Force output",
      "--list" -> ListTy(StringTy) << "List of input no",
      "--doubles" -> ListTy(DoubleTy) << "List of input doubles",
      ("-c" -> Flag) << "Use c compiler",
      ("-j" -> Flag) required
    )

    val arguments = Array("--list", "blup.txt", "hans.txt", "yeay.txt", "--heapsize", "blup", "-p", "5.34", "--pc", "10", "-f", "true",
    "--doubles", "4.5", "3.2", "0", "-c"
    )

    implicit val asReal = new ListStringConverter[Real] {
      override def convert(elem: List[String]): Real = Real(elem.head.toDouble)
    }

    using(configs:_*).parse(arguments:_*) { context =>
      val pm = context.as[Real]("-p")
      val pc = context.get[Int]("--pc")
      val b = context.as[Boolean]("-f")
      val list = context.as[List[String]]("--list")
      val doubles = context.as[List[Double]]("--doubles")
      val cFlag = context.as[Boolean]("-c")
      val jFlag = context.get[Boolean]("-j")

      println(s"pm  $pm pc $pc boolean $b\n list $list")
      println(s"Double list $doubles")
      println(s"cflag $cFlag jflag $jFlag")
      println(context)
    }
  }

}
