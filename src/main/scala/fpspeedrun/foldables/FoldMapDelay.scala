package fpspeedrun
package foldables

import cats.{Eval, Foldable, Monoid}
import newts.Dual
import syntax.delay._
import cats.syntax.semigroup._

abstract class FoldMapDelay[F[_]] extends Foldable[F] {
  def foldMapDelay[A, B: Monoid: Delay](fa: F[A])(f: A => B): B

  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    foldMapDelay[A, Dual[LzEndo[B]]](fa)(a => Dual(LzEndo(_.map(f(_, a))))).getDual.run(Eval.now(b)).value
  override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    foldMapDelay[A, LzEndo[B]](fa)(a => LzEndo(be => f(a, be))).run(lb)
}

object FoldMapDelay {

  implicit val streamInstance: Foldable[Stream] = new FoldMapDelay[Stream] {
    override def foldMapDelay[A, B: Monoid: Delay](fa: Stream[A])(f: A => B): B =
      fa match {
        case Stream() => Monoid.empty[B]
        case _        => f(fa.head).delay |+| foldMapDelay(fa.tail)(f).delay
      }
  }
}
