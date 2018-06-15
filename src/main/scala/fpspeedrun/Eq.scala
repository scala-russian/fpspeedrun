package fpspeedrun

import fpspeedrun.syntax.eq._

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {
  implicit def compareList[T](implicit eq: Eq[T]): Eq[Seq[T]] = (first, second) =>
    if (first.size != second.size) false
    else first.zip(second).forall { case (x, y) => x === y }
}