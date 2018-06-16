package fpspeedrun.syntax

object compare {
    implicit class CompareOps[T](val x: T) extends AnyVal {
      def compare(y: T)(implicit ord: Ord[T]): Ord.Compare = ord.compare(x, y)
    }
  }