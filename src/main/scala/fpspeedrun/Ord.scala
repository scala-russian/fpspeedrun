package fpspeedrun

import fpspeedrun.syntax.ord._

import scala.annotation.tailrec

trait Ord[T] extends Eq[T] {
  def compare(x: T, y: T): Ord.Compare
}

object Ord {

  sealed trait Compare

  object Compare {

    case object LT extends Ord.Compare //less than
    case object EQ extends Ord.Compare //equals to
    case object GT extends Ord.Compare //greater than
  }

  implicit def listOrd[A: Ord]: Ord[List[A]] = new Ord[List[A]] {
    override def compare(x: List[A], y: List[A]): Compare = {
      @tailrec def go(xs: List[A], ys: List[A]): Compare =
        (xs, ys) match {
          case (Nil, Nil) => Ord.Compare.EQ
          case (Nil, _ :: _) => Ord.Compare.LT
          case (_ :: _, Nil) => Ord.Compare.GT
          case (xh :: xt, yh :: yt) => xh <> yh match {
            case Compare.EQ => go(xt, yt)
            case r => r
          }
        }

      go(x, y)
    }

    override def ===(x: List[A], y: List[A]): Boolean = compare(x, y) == Ord.Compare.EQ
  }
}
