package fpspeedrun
import simulacrum.{op, typeclass}
import fpspeedrun.Ord.Compare._

import scala.annotation.tailrec

@typeclass
trait Integ[A] extends Num[A] {
  @op("/%", alias = true)
  def quotRem(x: A, y: A): (A, A)

  @op("/", alias = true)
  def quot(x: A, y: A): A = quotRem(x, y)._1

  @op("%", alias = true)
  def rem(x: A, y: A): A = quotRem(x, y)._2

  @tailrec private def gcdRun(x: A, y: A): A =
    if (equal(y, zero)) x
    else gcdRun(y, rem(x, y))

  def gcd(x: A, y: A): A = compare(x, y) match {
    case GT => gcdRun(x, y)
    case EQ => x
    case LT => gcdRun(y, x)
  }

  def lcm(x: A, y: A): A = times(quot(x, gcd(x, y)), y)
}

object Integ extends StdIntegInstances[Integ] {
  def fromIntegral[A](implicit int: Integral[A]): Integ[A] = new FromIntegral[A]

  class FromIntegral[A](implicit int: Integral[A]) extends Num.FromNumeric[A] with Integ[A] {
    override def quotRem(x: A, y: A): (A, A) = int.quot(x, y) -> int.rem(x, y)
    override def quot(x: A, y: A): A         = super.quot(x, y)
  }
}

trait StdIntegInstances[TC[typ] >: Integ[typ]] {
  final implicit val intInteg: TC[Int]       = Integ.fromIntegral
  final implicit val longInteg: TC[Long]     = Integ.fromIntegral
  final implicit val bigIntInteg: TC[BigInt] = Integ.fromIntegral
}
