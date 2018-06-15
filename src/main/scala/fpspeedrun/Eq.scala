package fpspeedrun

import scala.annotation.tailrec
import fpspeedrun.syntax.eq._

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {

  implicit def eqList[T](implicit eq: Eq[T]): Eq[Seq[T]] =
    (first, second) =>
      (first, second) match {
        case (l, r) if l.size != r.size => false
        case _                          => eqFunc(first, second)
    }

  @tailrec
  private def eqFunc[T](l1: Seq[T], l2: Seq[T])(implicit eq: Eq[T]): Boolean =
    (l1, l2) match {
      case (Nil, Nil) => true
      case (lHead :: lTail, rHead :: rTail) =>
        lHead === rHead && eqFunc(lTail, rTail)
    }
}
