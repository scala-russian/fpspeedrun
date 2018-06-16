package fpspeedrun

import fpspeedrun.Ord.Compare

final case class Ratio(numerator: Int, denumerator: Int)

object Ratio {
  implicit val compareRatioOrd: Ord[Ratio] = (x: Ratio, y: Ratio) => {
    (x.numerator * y.denumerator, x.denumerator * y.numerator) match {
      case (a, b) if a == b => Compare.EQ
      case (a, b) if a > b => Compare.GT
      case (a, b) if a < b => Compare.LT
    }
  }
}