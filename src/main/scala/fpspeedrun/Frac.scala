package fpspeedrun
import simulacrum.{op, typeclass}

@typeclass
trait Frac[A] extends Num[A] {
  @op("/", alias = true)
  def div(x: A, y: A): A

  @op(name = "unary_~", alias = true)
  def reciprocal(x: A): A = div(one, x)
}

object Frac extends StdFracInstances[Frac] {
  def fromFractional[A: Fractional]: Frac[A] = new FromFractional[A]

  class FromFractional[A](implicit frac: Fractional[A]) extends Num.FromNumeric[A] with Frac[A] {
    def div(x: A, y: A): A = frac.div(x, y)
  }
}

trait StdFracInstances[TC[typ] >: Frac[typ]] {
  final implicit val doubleFrac: TC[Double] = Frac.fromFractional
  final implicit val floatFrac: TC[Float] = Frac.fromFractional
  final implicit val bigDecimalFrac: TC[BigDecimal] = Frac.fromFractional
}


