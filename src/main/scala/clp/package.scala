import clp.config._

package object clp {
  import types._

  implicit def tupleToConfig[A](tup: (String, ParseTy)): Config = tup match {
    case (name:String, ty:ParseTy) => config.DefaultConfig(List(name), ty)
  }
}
