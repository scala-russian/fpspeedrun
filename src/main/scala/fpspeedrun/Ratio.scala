package fpspeedrun
import fpspeedrun.Ord.ordLong
import fpspeedrun.syntax.ord._

final case class Ratio(num: Int, denom: Int)

object Ratio {

  implicit val ord: Ord[Ratio] = new Ord[Ratio] {

    override def compare(x: Ratio, y: Ratio): Ord.Compare = {
      val left = x.num.toLong * y.denom
      val right = y.num.toLong * x.denom
      left <> right
    }

    override def ===(x: Ratio, y: Ratio): Boolean =
      compare(x, y) == Ord.Compare.EQ

  }
}
