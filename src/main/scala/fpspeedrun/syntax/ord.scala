package fpspeedrun.syntax

import fpspeedrun.Ord

object ord {
  implicit class OrdOps[T](val x: T) extends AnyVal {
    def ===(y: T)(implicit ord: Ord[T]): Boolean = ord.===(x, y)

    def =!=(y: T)(implicit ord: Ord[T]): Boolean = ord.=!=(x, y)

    def >(y: T)(implicit ord: Ord[T]): Boolean = ord.>(x, y)

    def <(y: T)(implicit ord: Ord[T]): Boolean = ord.<(x, y)

    def >=(y: T)(implicit ord: Ord[T]): Boolean = ord.>=(x, y)

    def <=(y: T)(implicit ord: Ord[T]): Boolean = ord.<=(x, y)

    def compare(y: T)(implicit ord: Ord[T]): Ord.Compare = ord.compare(x, y)
  }
}
