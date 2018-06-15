package fpspeedrun

object syntax {
  object eq {
    implicit class EqOps[T](val x: T) extends AnyVal {
      def ===(y: T)(implicit ev: Eq[T]): Boolean = implicitly[Eq[T]].===(x, y)
    }
  }

  object ord {
    implicit class OrdOps[T](val x: T) extends AnyVal {
      def <>(y: T)(implicit ev: Ord[T]): Ord.Compare = implicitly[Ord[T]].compare(x, y)
    }
  }
}
