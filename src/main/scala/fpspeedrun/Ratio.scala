package fpspeedrun

import fpspeedrun.Ord.Compare

final case class Ratio(numerator: Int, denominator: Int)

object Ratio {
  implicit val eqRatio: Eq[Ratio] = (x: Ratio, y: Ratio) => {
    x.numerator.toLong * y.denominator.toLong ==
      x.denominator.toLong * y.numerator.toLong
  }

  implicit val ordRatio: Ord[Ratio] = (x: Ratio, y: Ratio) => {
    (x.numerator.toLong * y.denominator.toLong, x.denominator.toLong * y.numerator.toLong) match {
      case (a, b) if a > b  => Compare.GT
      case (a, b) if a == b => Compare.EQ
      case (a, b) if a < b  => Compare.LT
    }
  }
}
