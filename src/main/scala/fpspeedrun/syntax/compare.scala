package fpspeedrun.syntax

import fpspeedrun.Ord
import fpspeedrun.Ord.Compare.{EQ, GT, LT}

object compare {
  implicit class CompareOps[T](val x: T) extends AnyVal {
    def compare(y: T)(implicit ord: Ord[T]): Ord.Compare = ord.compare(x, y)

    def >(y: T)(implicit ord: Ord[T]): Boolean = compare(y) == GT
    def <(y: T)(implicit ord: Ord[T]): Boolean = compare(y) == LT
    def >=(y: T)(implicit ord: Ord[T]): Boolean = {
      val c = compare(y)
      c == GT || c == EQ
    }
    def <=(y: T)(implicit ord: Ord[T]): Boolean = {
      val c = compare(y)
      c == LT || c == EQ
    }
  }
}