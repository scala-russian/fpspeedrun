package fpspeedrun

import fpspeedrun.Ord.Compare
import fpspeedrun.Ord.Compare._
import fpspeedrun.syntax.ord._

final case class Ratio(num: Int, denom: Int)

object Ratio {
  implicit var ordInt: Ord[Int] = new Ord[Int] {
    override def compare(x: Int, y: Int): Compare = (x, y) match {
      case _ if x > y => GT
      case _ if x < y => LT
      case _          => EQ
    }

    override def ===(x: Int, y: Int): Boolean = x == y
  }

  implicit val ordRatio: Ord[Ratio] = new Ord[Ratio] {
    override def compare(x: Ratio, y: Ratio): Compare =
      (x.num * y.denom) <> (y.num * x.denom)

    override def ===(x: Ratio, y: Ratio): Boolean =
      x.num * y.denom == y.num * x.denom
  }
}
