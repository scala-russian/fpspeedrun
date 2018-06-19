package fpspeedrun
import simulacrum.{op, typeclass}

import scala.annotation.tailrec

@typeclass
trait Num[A] extends Ord[A] {
  def fromInt(x: Int): A

  @op("+", alias = true)
  def plus(x: A, y: A): A

  @op(name = "*", alias = true)
  def times(x: A, y: A): A

  def zero: A     = fromInt(0)
  def one: A      = fromInt(1)
  def minusOne: A = fromInt(-1)

  @op("unary_!", alias = true)
  def negate(x: A): A = times(x, minusOne)

  @op("-", alias = true)
  def minus(x: A, y: A): A = plus(x, negate(y))

  def pow(x: A, p: Int): A = Num.fastPow(x, p)(this)
}

object Num extends StdNumInstances[Num] {
  import ops._

  def fastPow[A](x: A, p: Int)(implicit num: Num[A]): A = {
    @tailrec def go(p2: A, acc: A, p: Int): A =
      if (p == 0) acc
      else go(p2 * p2, if (p % 2 == 0) acc else acc * p2, p / 2)

    go(x, num.one, p)
  }

  def fromNumeric[A](implicit num: Numeric[A]): Num[A] = new FromNumeric[A]

  class FromNumeric[A](implicit num: Numeric[A]) extends Ord.FromOrdering[A] with Num[A] {
    override def fromInt(x: Int): A   = num.fromInt(x)
    override def plus(x: A, y: A): A  = num.plus(x, y)
    override def times(x: A, y: A): A = num.times(x, y)
    override val zero: A              = num.zero
    override val one: A               = num.one
    override def negate(x: A): A      = num.negate(x)
    override def minus(x: A, y: A): A = num.minus(x, y)
  }
}

trait StdNumInstances[TC[typ] >: Num[typ]] extends StdIntegInstances[TC] with StdFracInstances[TC]
