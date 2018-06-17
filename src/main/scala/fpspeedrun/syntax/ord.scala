package fpspeedrun.syntax

import fpspeedrun.Ord
import fpspeedrun.Ord.Compare
import fpspeedrun.Ord.Compare.{EQ, GT, LT}

object ord {
  implicit class OrdOps[T](val x: T) extends AnyVal {
    def <>(y: T)(implicit ord: Ord[T]): Compare = ord.compare(x, y)

    def <(y: T)(implicit ord: Ord[T]): Boolean  = ord.compare(x, y) == LT
    def <=(y: T)(implicit ord: Ord[T]): Boolean = List(EQ, LT).contains(ord.compare(x, y))
    def >(y: T)(implicit ord: Ord[T]): Boolean  = ord.compare(x, y) == GT
    def >=(y: T)(implicit ord: Ord[T]): Boolean = List(EQ, GT).contains(ord.compare(x, y))
  }
}
