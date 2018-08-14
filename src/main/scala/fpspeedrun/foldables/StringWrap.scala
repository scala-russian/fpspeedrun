package fpspeedrun.foldables

import cats.{Eval, Foldable, Monoid}
import fpspeedrun.foldables.LzEndo._
import newts.Dual

case class StringWrap[E](str: String)(implicit ev: Char =:= E) {
  def foldMap[B: Monoid](f: E => B): B = {
    str.map(ch => f(ev(ch))).foldLeft(Monoid[B].empty)(Monoid[B].combine)
  }
}

object StringWrap {
  implicit def foldable = new Foldable[StringWrap[?]] {

    override def foldLeft[A, B](fa: StringWrap[A], b: B)(f: (B, A) => B): B = {
      fa.foldMap[LzEndo[B]](a => LzEndo[B](_.map(f(_, a)))).run(Eval.now(b)).value
    }

    override def foldRight[A, B](fa: StringWrap[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      fa.foldMap[Dual[LzEndo[B]]](a => Dual(LzEndo(ev => f(a, ev)))).getDual.run(lb)
    }
  }
}


//object Main3 extends App {
//  import StringWrap._
//  import cats.syntax.foldable._
//
//  require(StringWrap("asdqwe").foldLeft("A") { (acc, v) => v + acc } == "ewqdsaA")
//  require(StringWrap("asdqwe").foldRight(Eval.now("A")) { (v, acc) => acc.map(_ + v) }.value == "Aewqdsa")
//}
