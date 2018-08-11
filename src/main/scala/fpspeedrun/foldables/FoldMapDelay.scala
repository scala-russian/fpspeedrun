package fpspeedrun
package foldables

import cats.{ Eval, Foldable, Monoid }
import fpspeedrun.foldables.LzEndo._
import syntax.delay._
import cats.syntax.semigroup._
import newts.Dual

abstract class FoldMapDelay[F[_]] extends Foldable[F] {
  def foldMapDelay[A, B: Monoid: Delay](fa: F[A])(f: A => B): B

  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = {
    foldMapDelay[A, LzEndo[B]](fa)(a => LzEndo[B](ev => ev.map(internal => f(internal, a))))
      .run(Eval.now(b)).value
  }
  override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    foldMapDelay[A, Dual[LzEndo[B]]](fa)(a => Dual(LzEndo[B](f.curried(a)))).getDual.run(lb)
  }
}

object FoldMapDelay {

  implicit val streamInstance: Foldable[Stream] = new FoldMapDelay[Stream] {
    override def foldMapDelay[A, B: Monoid : Delay](fa: Stream[A])(f: A => B): B = {
      fa.headOption.fold(Monoid[B].empty) { h =>
        f(h) |+| foldMapDelay(fa.tail)(f).delay
      }
    }
  }
}
