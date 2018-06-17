package fpspeedrun.syntax

import fpspeedrun.Semigroup

object semigroup {
  implicit class SemiOps[T](val x: T) extends AnyVal {
    def |+|(y: T)(implicit semi: Semigroup[T]): T = semi.combine(x, y)
  }
}
