package fpspeedrun

object syntax {
  object eq{
    implicit class EqOps[T](val x: T) extends AnyVal{
      def ===(y: T)(implicit eq: Eq[T]): Boolean = eq.===(x, y)
    }
  }
}
