package fpspeedrun

import fpspeedrun.Ord.{EQ, GT, LT}

final case class Ratio(numerator: Int, denominator: Int)

object Ratio {

  implicit val ratioOrd: Ord[Ratio] = (x: Ratio, y: Ratio) => {
    val crossProductsDiff: Int = x.numerator * y.denominator - x.denominator * y.numerator

    if (crossProductsDiff > 0) GT
    else if (crossProductsDiff < 0) LT
    else EQ
  }

}
