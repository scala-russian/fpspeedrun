package fpspeedrun

import fpspeedrun.Ord.Compare

final case class Ratio(num: Int, den: Int)

object Ratio {

  implicit val ord: Ord[Ratio] = new Ord[Ratio] {
    override def compare(x: Ratio, y: Ratio): Compare = {
      x.num * y.den - y.num * x.den match {
        case n if n < 0 => Ord.Compare.LT
        case n if n > 0 => Ord.Compare.GT
        case _ => Ord.Compare.EQ
      }
    }

    override def ===(x: Ratio, y: Ratio): Boolean = compare(x, y) == Ord.Compare.EQ
  }

  implicit val eq: Eq[Ratio] = (x, y) => ord.===(x, y)
}

