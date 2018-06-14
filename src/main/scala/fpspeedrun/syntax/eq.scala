package fpspeedrun.syntax

import fpspeedrun.lesson_1.Eq

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object eq {
  implicit class EqOps[T](val x: T) extends AnyVal {
    def ====(y: T)(implicit eq0: Eq[T]): Boolean = eq0.===(x, y)
    def =!=(y: T)(implicit eq0: Eq[T]): Boolean = ! ====(y)
  }
}
