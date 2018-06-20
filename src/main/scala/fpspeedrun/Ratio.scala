package fpspeedrun

import fpspeedrun.syntax.eq._

final case class Ratio(num: Int, den: Int)

object Ratio {
  //  implicit val eq: Eq[Ratio] = (x, y) => x.num.toLong * y.den === x.den.toLong * y.num
  implicit val ord: Ord[Ratio] = new Ord[Ratio] {
    override def compare(x: Ratio, y: Ratio): Ord.Compare = ???

    override def ===(x: Ratio, y: Ratio): Boolean =
      x.num.toLong * y.den === x.den.toLong * y.num
  }
}

