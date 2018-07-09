package fpspeedrun
package foldables
import cats.syntax.foldable._
import cats.{Eval, Foldable}
import fpspeedrun.foldables.FoldMapDelay.streamInstance
import fpspeedrun.syntax.num.{zero, _}


object FoldMapDelayApp {
  def sum[F[_]: Foldable, A: Num](fa: F[A]): A = fa.foldLeft(zero)(_ + _)

  def sumN[F[_]: Foldable, A: Num](n: Int)(fa: F[A]): A =
    fa.foldRight[Int => Eval[A]](Eval.now(_ => Eval.now(zero)))((x, ef) =>
      Eval.later {
        case 0 => Eval.now(zero)
        case n =>
          for {
            f <- ef
            s <- f(n - 1)
          } yield s + x
      })
      .flatMap(_(n))
      .value

  def main(args: Array[String]): Unit = {
//    println(sum(Stream.range[BigInt](0, 100001)))
//    println(sumN(5)(Stream.range(0, 11).map{x => println(x); x * 2 + 1}))
//    println(sumN(10001)(Stream.from(0)))
  }
}
