package fpspeedrun

object syntax {
  object eq {
    implicit class EqOps[T](val x: T) extends AnyVal {
      def ===(y: T)(implicit eq: Eq[T]): Boolean = eq.===(x, y)
    }
  }

  object ord {
    implicit class OrdOps[T](val x: T)(implicit ord: Ord[T]) {

      def <>(y: T): Ord.Compare = ord.compare(x, y)

      def <(y: T): Boolean = (x <> y) == Ord.Compare.LT

      def >(y: T): Boolean = (x <> y) == Ord.Compare.GT

      def <=(y: T): Boolean = !(x > y)

      def >=(y: T): Boolean = !(x < y)

    }
  }
}
