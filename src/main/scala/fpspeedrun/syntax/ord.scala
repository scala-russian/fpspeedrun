package fpspeedrun.syntax

import fpspeedrun.Ord
import fpspeedrun.Ord.Compare

object ord {

  implicit class OrdOps[T](val x: T) extends AnyVal {
    def <>(y: T)(implicit ord: Ord[T]): Compare = ord.compare(x, y)
  }

}
