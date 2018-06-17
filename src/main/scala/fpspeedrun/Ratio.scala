package fpspeedrun

import fpspeedrun.Ord.Compare
import fpspeedrun.syntax.ord._
import syntax.eq._

final case class Ratio(num: Int, den: Int)

object Ratio {
  implicit val eq: Eq[Ratio] = (x, y) => x.num.toLong * y.den === x.den.toLong * y.num

  implicit val ratioOrd: Ord[Ratio] = new Ord[Ratio] {
    override def compare(x: Ratio, y: Ratio): Compare =
      (x.num * y.den) <> (y.num * x.den)

    override def ===(x: Ratio, y: Ratio): Boolean =
      x.num * y.den == y.num * x.den
  }

  def sum(a: Ratio, b: Ratio): Ratio      = new Ratio(a.num * b.den + b.num * a.den, a.den * b.den)
  def multiply(a: Ratio, b: Ratio): Ratio = new Ratio(a.num * b.num, a.den * b.den)

  implicit val ratioSumMonoid: Monoid[Sum[Ratio]] = new Monoid[Sum[Ratio]] {
    override def empty: Sum[Ratio]                                 = Sum(Ratio(0, 1))
    override def combine(x: Sum[Ratio], y: Sum[Ratio]): Sum[Ratio] = Sum(sum(x.x, y.x))
  }

  implicit val ratioProdMonoid: Monoid[Prod[Ratio]] = new Monoid[Prod[Ratio]] {
    override def empty: Prod[Ratio]                                   = Prod(Ratio(1, 1))
    override def combine(x: Prod[Ratio], y: Prod[Ratio]): Prod[Ratio] = Prod(multiply(x.x, y.x))
  }

  object syntax {
    implicit class RatioOps(val x: Ratio) extends AnyVal {
      def +(y: Ratio): Ratio = Ratio.sum(x, y)
      def *(y: Ratio): Ratio = Ratio.multiply(x, y)
    }
  }
}
