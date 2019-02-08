package fpspeedrun
package foldables
import cats.{Eval, Foldable}
import fpspeedrun.syntax.num.zero
import cats.syntax.foldable._
import syntax.num._
import FoldMapDelay.streamInstance


object FoldMapDelayApp {
  def sum[F[_]: Foldable, A: Numeric](fa: F[A]): A = fa.foldLeft(zero)(_ + _)

  def sumN[F[_]: Foldable, A: Numeric](n: Int)(fa: F[A]): A =
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
//    println(Eval.now(println("kek")).delay)
    println(sum(Stream.range(0, 100000)))
//    println(sum(Stream.range(0, 11).map{x => println(x); x * 2 + 1}))
//    println(sumN(5)(Stream.range(0, 11).map{x => println(x); x * 2 + 1}))
  }
}
