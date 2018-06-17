package fpspeedrun.lesson_1

import fpspeedrun.lesson_1.Ord.Compare.{EQ, GT, LT}
import fpspeedrun.lesson_2.{Monoid, Prod, SemiGroup, Sum}

import scala.annotation.tailrec

final case class Ratio(n: Int, d: Int) {
  require(d != 0)

  private val min = gcd(n, d)
  val numer = n / min
  val denom = d / min

  @tailrec
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }
}

case object Ratio {

  val empty: Ratio = Ratio(0, 1)

  implicit val eqRatio: Eq[Ratio] = (x: Ratio, y: Ratio) => (x.numer * Math.sqrt(x.denom)) == (y.numer * Math.sqrt(y.denom))

  implicit val compareRatio: Ord[Ratio] = (x: Ratio, y: Ratio) =>
    (x.numer * Math.sqrt(x.denom)) -> (y.numer * Math.sqrt(y.denom)) match {
      case (l, r) if l > r => GT
      case (l, r) if l < r => LT
      case _ => EQ
    }

  implicit val sumSemi: SemiGroup[Ratio] = (x: Ratio, y: Ratio) => sum(x, y)

//  implicit val prodSemi: SemiGroup[Ratio] = (x: Ratio, y: Ratio) => prod(x, y)

  implicit val ratioSumSemi: SemiGroup[Sum[Ratio]] = (x: Sum[Ratio], y: Sum[Ratio]) => Sum(sum(x.v, y.v))

  implicit val ratioProdSemi: SemiGroup[Prod[Ratio]] = (x : Prod[Ratio], y: Prod[Ratio]) => Prod(prod(x.v, y.v))

  implicit val ratioSumMonoid: Monoid[Sum[Ratio]] = new Monoid[Sum[Ratio]] {
    override def empty: Sum[Ratio] = Sum(Ratio.empty)

    override def combine(x: Sum[Ratio], y: Sum[Ratio]): Sum[Ratio] = Sum(sum(x.v, y.v))
  }

  implicit val ratioProdMonoid: Monoid[Prod[Ratio]] = new Monoid[Prod[Ratio]] {
    override def empty: Prod[Ratio] =  Prod(Ratio.empty)

    override def combine(x: Prod[Ratio], y: Prod[Ratio]): Prod[Ratio] = Prod(prod(x.v, y.v))
  }

  def sum(x: Ratio,y: Ratio): Ratio = new Ratio(
    x.numer * y.denom + y.numer * x.denom, x.denom * y.denom
  )

  def prod(x: Ratio,y: Ratio): Ratio = new Ratio(x.numer * y.numer, x.denom * y.denom)
}