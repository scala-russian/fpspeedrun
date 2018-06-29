import cats.Eval
import fpspeedrun.Fooldable._
import fpspeedrun.Fooldable.ops._
import cats.instances.int._
import cats.instances.bigInt._

object CustomFolds {

  def main(args: Array[String]): Unit = {
    val s = Stream.from(1)
    println(s.take(100000).map(BigInt(_)).foldMapLazy(x => Eval.now(x)).value)
    println(s.take(100000).foldl("0")(_ + _.toString).take(50))
    println(s.take(100000).foldr(Eval.now("x")) { (a, lb) =>
      lb.map(_ + a.toString)
    }.value.take(50))

    println(s.map(BigInt(_)).sumN(100000))
  }
}
