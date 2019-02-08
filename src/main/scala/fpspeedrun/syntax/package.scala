package fpspeedrun
package syntax

import cats.Functor
import fpspeedrun.foldables.Delay

object num extends Numeric.ExtraImplicits {
  def zero[A](implicit A: Numeric[A]) = A.zero
  def one[A](implicit A: Numeric[A]) = A.one
  def fromInt[A](x: Int)(implicit A: Numeric[A]) = A.fromInt(x)
}

object delay extends Delay.ToDelayOps {
  implicit class DelayOps[A](x: => A) {
    def delay(implicit delay: Delay[A]): A = delay.delay(x)
  }
}

object functor extends Functor.ToFunctorOps {
  implicit class FunctorFunctionOps[A, B](val f: A => B) extends AnyVal {
    def <#>[F[_]](fa: F[A])(implicit F: Functor[F]): F[B] = ???
  }


}
