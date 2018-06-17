package fpspeedrun.syntax

import fpspeedrun.lesson_2.Monoid

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object monoid {
  implicit class MonoidOps[T](val x: T)  extends AnyVal {
    def combine[T](y: T)(implicit m: Monoid[T]): T =  m.combine(x, y)
    def empty[T](y: T)(implicit m: Monoid[T]): T =  m.empty
  }
}
