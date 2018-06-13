package fpspeedrun

final case class Ratio(num: Int, denom: Int)

object Ratio {

  implicit val eq: Eq[Ratio] = (l, r) =>
    l.num.toLong * r.denom == r.num.toLong * l.denom

  implicit val ord: Ord[Ratio] = new Ord[Ratio] {

    override def compare(x: Ratio, y: Ratio): Ord.Compare = {
      val left = x.num.toLong * y.denom
      val right = y.num.toLong * x.denom
      if (left == right) {
        Ord.Compare.EQ
      } else if (left > right) {
        Ord.Compare.GT
      } else Ord.Compare.LT
    }

    override def ===(x: Ratio, y: Ratio): Boolean = Ratio.eq.===(x, y)

  }
}
