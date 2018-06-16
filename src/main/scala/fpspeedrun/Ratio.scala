package fpspeedrun

<<<<<<< HEAD
import syntax.eq._

final case class Ratio(num: Int, den: Int)

object Ratio {
  implicit val eq: Eq[Ratio] = (x, y) => x.num.toLong * y.den === x.den.toLong * y.num
}
=======
import fpspeedrun.Ord.Compare._
>>>>>>> FirstImpl

final case class Ratio(numer: Int, denom: Int)

object Ratio {
  implicit val eqRatio: Eq[Ratio] =
    (x: Ratio, y: Ratio) => x.numer * y.denom == y.numer * x.denom

  implicit val ordRatio: Ord[Ratio] = new Ord[Ratio] {
    override def ===(x: Ratio, y: Ratio): Boolean = eqRatio.===(x, y)

    override def compare(x: Ratio, y: Ratio): Ord.Compare =
      if (===(x, y)) EQ
      else if (x.numer * y.denom > y.numer * x.denom) GT
      else LT
  }
}