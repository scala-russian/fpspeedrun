package fpspeedrun

import scala.annotation.tailrec

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

  def fromEquals[T]: Eq[T] = _ == _

  implicit val intEq: Eq[Int]   = fromEquals
  implicit val longEq: Eq[Long] = fromEquals

  /** простой вариант */
  implicit def vectorEq[A: Eq]: Eq[Vector[A]] =
    (xs, ys) => xs.view.zip(ys).forall { case (x, y) => x === y }

  /** я у мамы оптимизатор */
  implicit def listEq[A: Eq]: Eq[List[A]] = {
    @tailrec def go(xs: List[A], ys: List[A]): Boolean =
      xs match {
        case Nil => ys.isEmpty
        case x :: xt =>
          ys match {
            case Nil     => false
            case y :: yt => x === y && go(xt, yt)
          }
      }
    go
  }

  implicit def orderList[T](implicit ord: Ord[T]): Ord[List[T]] = new Ord[List[T]] {
    override def compare(first: List[T], second: List[T]): Compare = {
      if (first.size < second.size) return LT
      if (first.size > second.size) return GT
      first
        .zip(second)
        .collectFirst {
          case (x, y) if (x <> y) != EQ => x <> y
        }
        .getOrElse(EQ)
    }

    override def ===(x: List[T], y: List[T]): Boolean = x.size == y.size && x.zip(y).forall {
      case (a, b) => a === b
    }
  }
}
