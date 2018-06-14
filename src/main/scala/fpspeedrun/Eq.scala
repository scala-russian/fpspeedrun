package fpspeedrun

import fpspeedrun.Ord.Compare
import fpspeedrun.Ord.Compare._
import syntax.eq._
import syntax.ord._

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {
  implicit def compareList[T](implicit eq: Eq[T]): Eq[List[T]] =
    (first, second) =>
      first.size == second.size && first.zip(second).forall {
        case (x, y) => x === y
    }

  implicit def orderList[T](implicit ord: Ord[T]): Ord[List[T]] = new Ord[List[T]] {
    override def compare(first: List[T], second: List[T]): Compare = {
      if (first.size < second.size) return LT
      if (first.size > second.size) return GT
      first
        .zip(second)
        .foldRight(EQ.asInstanceOf[Compare]) {
          case ((x, y), EQ) => if ((x <> y) != EQ) x <> y else EQ
          case (_, eq)      => eq
        }
    }

    override def ===(x: List[T], y: List[T]): Boolean = x.size == y.size && x.zip(y).forall {
      case (a, b) => a === b
    }
  }
}
