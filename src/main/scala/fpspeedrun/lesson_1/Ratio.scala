package fpspeedrun.lesson_1

import fpspeedrun.lesson_1.Ord.Compare.{EQ, GT, LT}

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

  implicit val eqRatio: Eq[Ratio] = (x: Ratio, y: Ratio) => (x.numer * Math.sqrt(x.denom)) == (y.numer * Math.sqrt(y.denom))

  implicit val compareRatio: Ord[Ratio] = (x: Ratio, y: Ratio) =>
    (x.numer * Math.sqrt(x.denom)) -> (y.numer * Math.sqrt(y.denom)) match {
      case (l, r) if l > r => GT
      case (l, r) if l < r => LT
      case _ => EQ
    }
}