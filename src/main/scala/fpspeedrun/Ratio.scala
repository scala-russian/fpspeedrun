package fpspeedrun

import syntax.eq._

final case class Ratio(num: Int, den: Int)

object Ratio {
  implicit val eq: Eq[Ratio] = (x, y) => x.num.toLong * y.den === x.den.toLong * y.num
}

