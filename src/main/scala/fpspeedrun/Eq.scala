package fpspeedrun

import syntax.eq._

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {
  implicit def compareList[T](implicit eq: Eq[T]): Eq[List[T]] =
    (first, second) =>
      first.size == second.size && first.zip(second).forall {
        case (x, y) => x === y
    }
}
