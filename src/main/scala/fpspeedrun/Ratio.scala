package fpspeedrun

import fpspeedrun.Ord.Compare._

final case class Ratio(numer: Int, denom: Int)

object Ratio {

  implicit val ordRatio: Ord[Ratio] = new Ord[Ratio] {
    override def ===(x: Ratio, y: Ratio): Boolean =
      x.numer * y.denom == y.numer * x.denom

    override def compare(x: Ratio, y: Ratio): Ord.Compare =
      if (===(x, y)) EQ
      else if (x.numer * y.denom > y.numer * x.denom) GT
      else LT
  }

  // semigroup instances
  implicit val combineRatio: SemiGroup[Sum[Ratio]] =
    (x: Sum[Ratio], y: Sum[Ratio]) => Sum(sum(x.x, y.x))
  implicit val mulCombineRatio: SemiGroup[Prod[Ratio]] =
    (x: Prod[Ratio], y: Prod[Ratio]) => Prod(mul(x.x, y.x))

  // monoid instances
  implicit val defaultMonoidRatio: Monoid[Ratio] = new Monoid[Ratio] {
    override def empty: Ratio = Ratio(0, 1)
    override def combine(x: Ratio, y: Ratio): Ratio = sum(x, y)
  }
  implicit val sumMonoidRatio: Monoid[Sum[Ratio]] = new Monoid[Sum[Ratio]] {
    override def empty: Sum[Ratio] = Sum(Ratio(0, 1))
    override def combine(x: Sum[Ratio], y: Sum[Ratio]): Sum[Ratio] = Sum(sum(x.x, y.x))
  }
  implicit val mulMonoidRatio: Monoid[Prod[Ratio]] = new Monoid[Prod[Ratio]] {
    override def empty: Prod[Ratio] = Prod(Ratio(1, 1))
    override def combine(x: Prod[Ratio], y: Prod[Ratio]): Prod[Ratio] = Prod(mul(x.x, y.x))
  }

  def sum(x: Ratio, y: Ratio): Ratio = {
    val num = x.numer * y.denom + x.denom * y.numer
    val denom = x.denom * y.denom
    lazy val g = gcd(num, denom)
    Ratio(num / g, denom / g)
  }

  def mul(x: Ratio, y: Ratio): Ratio = {
    val num = x.numer * y.numer
    val denom = x.denom * y.denom
    lazy val g = gcd(num, denom)
    Ratio(num/g, denom/g)
  }

  def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
}