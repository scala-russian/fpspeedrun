package fpspeedrun

import fpspeedrun.Ord.Compare
import fpspeedrun.syntax.ord._
import syntax.eq._

final case class Ratio(num: Int, den: Int)

object Ratio {
  implicit val eq: Eq[Ratio] = (x, y) => x.num.toLong * y.den === x.den.toLong * y.num

  implicit val ordRatio: Ord[Ratio] = new Ord[Ratio] {
    override def compare(x: Ratio, y: Ratio): Compare =
      (x.num * y.den) <> (y.num * x.den)

    override def ===(x: Ratio, y: Ratio): Boolean =
      x.num * y.den == y.num * x.den
  }
}
