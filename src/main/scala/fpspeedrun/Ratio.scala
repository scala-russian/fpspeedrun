package fpspeedrun

import fpspeedrun.Ord.Compare
import fpspeedrun.Ord.Compare._

final case class Ratio(num: Int, denom: Int)

object Ratio {
  implicit val eq: Eq[Ratio] = (fst, snd) =>
    fst.num * snd.denom == fst.denom * snd.num

  implicit val ord: Ord[Ratio] = new Ord[Ratio] {
    override def compare(x: Ratio, y: Ratio): Compare = (x, y) match {
      case _ if x.num * y.denom > y.num * x.denom => GT
      case _ if x.num * y.denom < y.num * x.denom => LT
      case _                                      => EQ
    }

    override def ===(x: Ratio, y: Ratio): Boolean =
      x.num * y.denom == y.num * x.denom
  }
}
