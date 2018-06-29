package fpspeedrun

import cats.Eval
import cats.Eval._
import fpspeedrun.Fooldable.LazyMonoid
import simulacrum.typeclass

@typeclass
trait Fooldable[F[_]] {
  def foldMapLazy[A, B: LazyMonoid](fa: F[A])(f: A => Eval[B]): Eval[B]
  def foldl[A, B](fa: F[A], acc: B)(f: (B, A) => B): B = {
    implicit val combiner: cats.Monoid[B => Eval[B]] = new cats.Monoid[B => Eval[B]] {
      override def empty: B => Eval[B] = Eval.now
      override def combine(x: B => Eval[B], y: B => Eval[B]): B => Eval[B] = b => Eval.defer(x(b).flatMap(y(_)))
    }
    foldMapLazy(fa) { a =>
      Eval.now {
        b: B => Eval.now(f(b, a))
      }
    }.flatMap(_ (acc)).value
  }

  def foldr[A, B](fa: F[A], lacc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    implicit val combiner: cats.Monoid[Eval[Eval[B] => Eval[B]]] = new cats.Monoid[Eval[Eval[B] => Eval[B]]] {
      override def empty: Eval[Eval[B] => Eval[B]] = Eval.now(identity)

      override def combine(x: Eval[Eval[B] => Eval[B]], y: Eval[Eval[B] => Eval[B]]): Eval[Eval[B] => Eval[B]] =
        Eval.later { b: Eval[B] =>
          for {
            xf <- x
            yf <- y
            xx <- xf(yf(b))
          } yield xx
        }
    }

    foldMapLazy[A, Eval[B] => Eval[B]](fa) { a =>
      Eval.now { lb: Eval[B] =>
        f(a, lb)
      }
    }.flatMap(_ (lacc))
  }

  def sumN[A: cats.Monoid](xs: F[A])(n: Int): A =
    foldr[A, Int => Eval[A]](xs, Eval.now(_ => Eval.now(cats.Monoid[A].empty))) { (x, acc) =>
      Eval.later { k: Int =>
        if (k == 0) Eval.now(cats.Monoid[A].empty)
        else for {
          f <- acc
          s <- f(k - 1)
        } yield cats.Monoid[A].combine(s, x)
      }
    }.flatMap(_ (n)).value
}

object Fooldable {
  type LazyMonoid[A] = cats.Monoid[Eval[A]]

  implicit val streamFooldable: Fooldable[Stream] = new Fooldable[Stream] {
    override def foldMapLazy[A, B: LazyMonoid](fa: Stream[A])(f: A => Eval[B]): Eval[B] =
      if (fa.isEmpty) cats.Monoid[Eval[B]].empty
      else cats.Monoid[Eval[B]].combine(f(fa.head), Eval.defer(foldMapLazy(fa.tail)(f)))
  }
}
