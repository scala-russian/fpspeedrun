package fpspeedrun

import fpspeedrun.Ord.{Compare, EQ}

trait Ord[T] extends Eq[T] {
  override def ===(x: T, y: T): Boolean = compare(x, y) == EQ
  def compare(x: T, y: T): Compare
}

object Ord {

  sealed trait Compare

  case object LT extends Compare //less than
  case object EQ extends Compare //equals to
  case object GT extends Compare //greater than

  import syntax.ord.OrdOps

  implicit def lexicographicalListOrd[T : Ord]: Ord[List[T]] = (x: List[T], y: List[T]) =>
    x zip y find { case (a, b) => a !== b } match {
      case Some((a, b)) => a compare b
      case None         => if (x.size < y.size) LT else if (x.size > y.size) GT else EQ
    }

}
