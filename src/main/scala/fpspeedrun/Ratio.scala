package fpspeedrun

import fpspeedrun.Ord.compareLong
import fpspeedrun.syntax.ord._

final case class Ratio(num: Long, denom: Long)

object Ratio{
  implicit val ord: Ord[Ratio] = (x:Ratio, y: Ratio) =>
    x.num * y.denom <> x.denom * y.num
}