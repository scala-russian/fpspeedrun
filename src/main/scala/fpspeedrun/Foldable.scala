package fpspeedrun

import cats.Eval
import simulacrum.typeclass

@typeclass
trait Foldable[F[_]] {
  type LazyMonoid[A] = Monoid[Eval[A]]

  def foldMapLazy[A, B: LazyMonoid](fa: F[A])(f: A => Eval[B]): Eval[B]

  def foldRightLazy[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {

    type EvalEndo = Eval[B] => Eval[B]

    implicit def evalEndoMonoid: LazyMonoid[EvalEndo] = new Monoid[Eval[EvalEndo]] {
      override def empty: Eval[EvalEndo] = Eval.later(identity[Eval[B]])

      override def combine(lx: Eval[EvalEndo], ly: Eval[EvalEndo]): Eval[EvalEndo] =
        Eval.later { e: Eval[B] => (for { x <- lx; y <- ly } yield x compose y).flatMap(_(e)) }
    }

    val folder: A => Eval[EvalEndo] = (a: A) => Eval.later { e: Eval[B] => f(a, e) }

    foldMapLazy[A, EvalEndo](fa)(folder).flatMap(_(lb))
  }

  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = {

    type Endo = B => B

    implicit def endoMonoid: Monoid[Eval[B => B]] = new Monoid[Eval[B => B]] {
      override def empty: Eval[B => B] = Eval.later(identity[B])

      override def combine(lx: Eval[B => B], ly: Eval[B => B]): Eval[B => B] =
        Eval.later { e: B => (for { x <- lx; y <- ly } yield x compose y).value(e) }
    }

    foldMapLazy[A, B => B](fa)({ a: A => Eval.later((b: B) => f(b, a)) }).value(b)
  }
}

object Foldable {

  implicit def streamFoldable: Foldable[Stream] = new Foldable[Stream] {
    override def foldMapLazy[A, B: LazyMonoid](fa: Stream[A])(f: A => Eval[B]): Eval[B] =
      Eval.defer {
        implicitly[LazyMonoid[B]].combine(f(fa.head), foldMapLazy(fa.tail)(f))
      }
  }

}