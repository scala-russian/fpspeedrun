package fpspeedrun

import fpspeedrun.Ord.Compare
import fpspeedrun.Ord.Compare._

trait Ord[T] extends Eq[T] {
  def compare(x: T, y: T): Compare
}

object Ord {
  sealed trait Compare
  object Compare {
    case object LT extends Compare  // less than
    case object EQ extends Compare  // equals to
    case object GT extends Compare  // greater than
  }

  implicit def compareLists[T](implicit ord: Ord[T]): Ord[List[T]] =
    new Ord[List[T]] {
      override def compare(x: List[T], y: List[T]): Compare =
        x.lengthCompare(y.length) match {
          case neg if neg < 0 => LT
          case zer if zer == 0 => EQ
          case _ => GT
        }

      import syntax.eq._
      override def ===(x: List[T], y: List[T]): Boolean = x === y
    }
}