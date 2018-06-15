package fpspeedrun

import fpspeedrun.Ord.Compare

object syntax {
  object eq{
    implicit class EqOps[T](val x: T) extends AnyVal{
      def ===(y: T)(implicit eq: Eq[T]): Boolean = eq.===(x, y)
    }
  }

  object ord{
    implicit class OrdOps[T](val x: T) extends AnyVal{
      def <>(y: T)(implicit ord: Ord[T]): Compare = ord.compare(x, y)
    }
  }
}
