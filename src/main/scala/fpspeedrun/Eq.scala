package fpspeedrun
import syntax.eq._

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {
  implicit def compareLists[T](implicit eqq: Eq[T]): Eq[List[T]] = {
    (x: List[T], y: List[T]) => x.nonEmpty && x.lengthCompare(y.length) == 0 &&
      x.zip(y).forall { case (f, s) => f === s }
  }
}
