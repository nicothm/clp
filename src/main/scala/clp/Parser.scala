package clp
import types._
import config._
import context._

import scala.annotation.tailrec
import clp.util._
class Parser(private val map:Map[String, Config]) {
  val definedFlag = "true"
  val notDefined = "false"

  //help-msg strings
  private lazy val helpColumnSeparator = "    "
  private lazy val fstColDescr = "Arg"
  private lazy val secColDescr = "Type"
  private lazy val thdColDescr = "Required"
  private lazy val fthColDescr = "Descr"

  /**Create a column with the given argument for the help-text and fill up the rest until maxChars with whitespace.
   */
  private def createColumn(v:String, maxChars:Int)(columnSeparator:String) =
    if(v.length < maxChars) ((0 until maxChars - v.length).foldLeft(v) { case (acc, idx) => acc+" " } + columnSeparator)
    else v+columnSeparator

  /** Make some craziness for printing a beautiful help text.
    */
  private def printHelp(): Nothing = {
    println("Usage:")

    val distinct = map.groupBy { _._2 } map { case (config, keys) => config -> keys.keys.toList }

    //get the maximum-count of chars which is needed in each column
    val (maxSizeKeyColumn, maxSizeTypeColumn, maxSizeRequColumn, maxSizeDescColumn) =
      distinct.foldLeft(fstColDescr.length, secColDescr.length, thdColDescr.length, fthColDescr.length)
        { case ((skC, stC, srC, sdC), (config, keys)) =>
          (
            max(skC, keys.mkString(", ").length),
            max(stC, config.getType().toString.length),
            max(1, srC),
            max(sdC, config.description.length)
            )
        }

    //print description
    println(
      helpColumnSeparator +
      createColumn(fstColDescr, maxSizeKeyColumn)(helpColumnSeparator) +
      createColumn(secColDescr, maxSizeTypeColumn)(helpColumnSeparator) +
      createColumn(thdColDescr, maxSizeRequColumn)(helpColumnSeparator) +
      fthColDescr
    )

    for {
      (config, keys) <- distinct
      keysString = keys.mkString(", ")
      configTy = config.getType()
      configDesc = config.description
      required = config.isRequired()
    } {
      val keyColumn = createColumn(keysString, maxSizeKeyColumn)(helpColumnSeparator)
      val typeColumn = createColumn(configTy.toString, maxSizeTypeColumn)(helpColumnSeparator)
      val requColumn = createColumn(if(required) "y" else "n", maxSizeRequColumn)(helpColumnSeparator)
      val descColumn = configDesc

      //print row
      println(helpColumnSeparator+keyColumn+typeColumn+requColumn+descColumn)
    }
    sys.exit(0)
  }

  private def typeError[T](pt: ParseTy, k: String, v: T): Nothing = {
    println(s"The given value $v for $k doesn't match on type $pt!")
    printHelp()
  }

  private def namesToConf(names:List[String], target: List[String]) =
    names.map{ name => name -> target}.toMap

  @tailrec
  private def parse(args: List[String], acc: Map[String, List[String]] = Map()): Map[String, List[String]] = {
    if (!args.isEmpty) {
      val key = args.head
      map.getOrElse(key, printHelp) match {
        case DefaultConfig(names, ty:ListTy, _, _) =>
          val (values, rest) = args.tail.span(x => !x.startsWith("-"))

          if(ty.validate(values)) {
            val target = namesToConf(names, values)
            parse(rest, acc ++ target)
          } else typeError(ty, key, values)
        case DefaultConfig(names, FlagTy, _, _) =>
          val target = key -> List(definedFlag)
          parse(args.tail, acc + target)
        case DefaultConfig(names, ty:ParseTy, _, _) =>
          val value = args.tail.head

          if(ty.validate(value)) {
            val target = namesToConf(names, List(value))
            parse(args.drop(2), acc ++ target)

          } else typeError(ty, key, value)
        case _ =>
          printHelp()
      }
    } else acc
  }


  def parse(programArgs: String*)(fn: Context => Unit): Unit = {
    val res = parse(programArgs.toList)

    //all arguments parsed, check if all required parameter are set
    val requiredParametersSet = map.filter{ case (_, config) => config.isRequired() }.forall{ case (name, _) => res.contains(name) }
    if(!requiredParametersSet) {
      println("Not all required parameters are given!")
      printHelp()
    } else {
      //add all not-set flags
      val erg = (for {
        (key, config) <- map
        if !res.contains(key)
      } yield key -> List(notDefined))

      val cont = new DefaultContext(res ++ erg)
      fn(cont)
    }
  }
}

object Parser {
  def apply(args: Config*): Parser = {
    //because we all love for-comprehensions
    val richMap = (for{conf <- args
                       name <- conf.getNames()
                       tupel = name -> conf
    } yield tupel) toMap

    new Parser(richMap)
  }

  def using(args: Config*): Parser = apply(args: _*)

}
