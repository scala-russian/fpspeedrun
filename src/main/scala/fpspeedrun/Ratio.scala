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
}