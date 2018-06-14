package fpspeedrun

final case class Ratio(numerator: Int, denominator: Int)

object Ratio {
  implicit val eqRatio = new Eq[Ratio] {
    override def ===(x: Ratio, y: Ratio): Boolean = {
      x.numerator.toLong * y.denominator.toLong ==
        x.denominator.toLong * y.numerator.toLong
    }
  }
}
