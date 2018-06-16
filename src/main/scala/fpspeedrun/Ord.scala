package fpspeedrun

import fpspeedrun.Ord.Compare

trait Ord[T] extends Eq[T] {
  def compare(x: T, y: T): Compare

  override def ===(x: T, y: T): Boolean = compare(x, y) == Compare.EQ
  def >(x: T, y: T): Boolean = compare(x, y) == Compare.GT
  def <(x: T, y: T): Boolean = compare(x, y) == Compare.LT
  def >=(x: T, y: T): Boolean = ! <(x, y)
  def <=(x: T, y: T): Boolean = ! >(x, y)
}

object Ord {
  sealed trait Compare
  object Compare {
    case object LT extends Compare //less than
    case object EQ extends Compare //equals to
    case object GT extends Compare //greater than
  }
}
