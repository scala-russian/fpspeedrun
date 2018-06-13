package fpspeedrun

import scala.annotation.tailrec
import fpspeedrun.syntax.eq._

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {

  implicit def eqList[T](implicit eq: Eq[T]): Eq[Seq[T]] =
    (first, second) => eqFunc(first, second)

  @tailrec
  private def eqFunc[T](l1: Seq[T], l2: Seq[T])(implicit eq: Eq[T]): Boolean = {
    if (l1.size != l2.size) {
      false
    } else if (l1.isEmpty) {
      true
    } else {
      l1.head === l2.head && eqFunc(l1.tail, l2.tail)
    }
  }
}
