package fpspeedrun

import cats.Eval

trait Foldable2[F[_]] {
  type LazyMonoid[A] = Monoid[Eval[A]]
  type LazyEndo[A] = Eval[A] => Eval[A]

  def endoLazyMonoid[A] = new LazyMonoid[LazyEndo[A]] {
    override def empty: Eval[LazyEndo[A]] = Eval.now(x => x)
    override def combine(x: Eval[LazyEndo[A]], y: Eval[LazyEndo[A]]): Eval[LazyEndo[A]] = {
      Eval.later(t => {
        (for {
          xv <- x
          yv <- y
        } yield yv andThen xv).flatMap(_(t))
      })
    }
  }

  def foldMapLazy[A, B: LazyMonoid](fa: F[A])(f: A => Eval[B]): Eval[B]

  def foldRightLazy[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    val fu: A => Eval[Eval[B] => Eval[B]] = a => Eval.later(f.curried(a))
    foldMapLazy[A, Eval[B] => Eval[B]](fa)(fu)(endoLazyMonoid)
      .flatMap(_(lb))
  }
}

object StreamFoldable2 extends Foldable2[Stream] {
  override def foldMapLazy[A, B: StreamFoldable2.LazyMonoid](fa: Stream[A])(f: A => Eval[B]): Eval[B] = {
    Eval.now(fa).flatMap { s =>
      val m = implicitly[StreamFoldable2.LazyMonoid[B]]
      if (s.isEmpty) m.empty else m.combine(f(s.head), Eval.defer(foldMapLazy(s.tail)(f)))
    }
  }
}

object Main extends App {
  def sumN(xs: Stream[Int])(n: Int): Int = {
    StreamFoldable2.foldRightLazy[Int, Int => Eval[Int]](xs, Eval.now(_ => Eval.now(0))) { case (el, cntToSum) =>
      Eval.later(cnt => {
        if (cnt <= 0) {
          Eval.now(0)
        }
        else {
          for {
            f <- cntToSum
            v <- f(cnt - 1)
          } yield el + v
        }
      })
    }.flatMap(_(n)).value
  }

  println(sumN(Stream.from(1))(10000)) //50005000
}
