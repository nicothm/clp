package clp

package object types {
  val DefaultType:ParseTy = StringTy
  def Int:ParseTy = IntTy
  def Double:ParseTy = DoubleTy
  def Boolean:ParseTy = Bool
  def Bool:ParseTy = BooleanTy
  def Flag:ParseTy = FlagTy
}
