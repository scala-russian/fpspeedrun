package fpspeedrun.syntax

import fpspeedrun.lesson_2.SemiGroup

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object semigroup {
  implicit class SemiGroupOps[T](val x: T) extends AnyVal {
    def combine(y: T)(implicit semiGroup: SemiGroup[T]): T = semiGroup.combine(x, y)

    def |+|(y: T)(implicit semiGroup: SemiGroup[T]): T = combine(y)
  }
}
