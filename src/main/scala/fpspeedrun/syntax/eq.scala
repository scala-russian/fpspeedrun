package fpspeedrun.syntax

import fpspeedrun.Eq

object eq {
  implicit class EqOps[T](val x: T) extends AnyVal {
    def ===(y: T)(implicit eq: Eq[T]): Boolean = eq.===(x, y)

    def =!=(y: T)(implicit eq: Eq[T]): Boolean = eq.=!=(x, y)
  }
}
