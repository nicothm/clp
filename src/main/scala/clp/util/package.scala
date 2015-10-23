package clp

package object util {

  /** Returns the greates of the given 2 values.
    */
  def max[T <% Ordered[T]](a:T, b:T) = if(a > b) a else b

}
