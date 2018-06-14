package fpspeedrun.syntax

import fpspeedrun.lesson_1.Ord
import fpspeedrun.lesson_1.Ord.Compare
import fpspeedrun.lesson_1.Ord.Compare.{EQ, GT, LT}

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object ord {
  implicit class OrdOps[T](val x: T) extends AnyVal {
    def <>(y: T)(implicit ord: Ord[T]): Compare = ord.<>(x, y)
    def ===(y: T)(implicit ord: Ord[T]): Boolean = ord.===(x, y)
    def >(y: T)(implicit ord: Ord[T]): Boolean = ord.<>(x, y) == GT
    def <(y: T)(implicit ord: Ord[T]): Boolean = ord.<>(x, y) == LT
    def <=(y: T)(implicit ord: Ord[T]): Boolean = <(y) || ===(y)
    def >=(y: T)(implicit ord: Ord[T]): Boolean = >(y) || ===(y)
  }
}
