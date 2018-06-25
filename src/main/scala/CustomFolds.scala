import cats.{ Eval, Foldable }
import cats.kernel.Monoid
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.stream._

object CustomFolds {

  def customFoldLeft[F[_] : Foldable, A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    fa.foldRight[B => Eval[B]](Eval.now(Eval.now(_))) { (x, acc) =>
      Eval.later { innerAcc =>
        acc.flatMap(_ (f(innerAcc, x)))
      }
    }.flatMap(_ (b)).value

  def customFoldRight[F[_] : Foldable, A, B](fa: F[A], b: B)(f: (A, B) => B): B = {
    val customMonoid = new Monoid[Eval[B => Eval[B]]] {
      override def empty: Eval[B => Eval[B]] = Eval.now(Eval.now(_))

      override def combine(x: Eval[B => Eval[B]], y: Eval[B => Eval[B]]): Eval[B => Eval[B]] =
        Eval.later { acc =>
          for {
            yf <- y
            r <- yf(acc)
            xf <- x
            l <- xf(r)
          } yield l
        }
    }

    fa.foldMap { a =>
      Eval.now { acc: B =>
        Eval.now(f(a, acc))
      }
    }(customMonoid).flatMap(_ (b)).value
  }

  def main(args: Array[String]): Unit = {
    val l = "1" :: "2" :: "3" :: "4" :: Nil
    println(customFoldLeft(l, "0")((acc, x) => acc + x))
    println(customFoldRight(l, "5")((x, acc) => acc + x))

    val s = Stream.from(1).map(BigInt(_))
    println(customFoldLeft(s.take(100000), "0")((acc, x) => acc + x.toString).take(100))
    println(customFoldRight(s.take(100000), "100001")((x, acc) => acc + x.toString).take(100))

  }
}
