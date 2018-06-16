package fpspeedrun

final case class Ratio(num: Long, den: Long)


object Ratio {
  implicit val ratioOrd: Ord[Ratio] = (x: Ratio, y: Ratio) => {
    val left = x.num * y.den
    val right = x.den * y.num
    left - right match {
      case 0 => Ord.Compare.EQ
      case diff if diff < 0 => Ord.Compare.LT
      case _ => Ord.Compare.GT
    }
  }

  implicit val ratioSumSg: SemiGroup[Sum[Ratio]] = (x: Sum[Ratio], y: Sum[Ratio]) =>
    Sum(sum(x.x, y.x))
  implicit val ratioProdSg: SemiGroup[Prod[Ratio]] = (x: Prod[Ratio], y: Prod[Ratio]) =>
    Prod(mul(x.x, y.x))

  def gcd(a: Long, b: Long): Long =
    if (b == 0) a
    else gcd(b, a % b)

  def sum(x: Ratio, y: Ratio): Ratio = {
    val num = x.num * y.den + x.den * y.num
    val den = x.den * y.den
    val foundGcd = gcd(num, den)
    Ratio(num / foundGcd, den / foundGcd)
  }

  def mul(x: Ratio, y: Ratio): Ratio = {
    val num = x.num * y.num
    val den = x.den * y.den
    val foundGcd = gcd(num, den)
    Ratio(num / foundGcd, den / foundGcd)
  }
}
