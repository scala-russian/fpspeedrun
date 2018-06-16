package fpspeedrun.syntax

import fpspeedrun.SemiGroup

object semigroup {
  implicit class SemiGroupOps[T](val x: T) extends AnyVal {
    def combine(y: T)(implicit sg: SemiGroup[T]): T = sg.combine(x, y)

    def |+|(y: T)(implicit sg: SemiGroup[T]): T = combine(y)
  }
}
