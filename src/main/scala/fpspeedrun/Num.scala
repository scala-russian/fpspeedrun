package fpspeedrun
import simulacrum.{ typeclass}

@typeclass
trait Num[A] extends Calc[A] with Ord[A]

object Num  extends StdNumInstances[Num] {

  def fromNumeric[A](implicit num: Numeric[A]): Num[A] = new FromNumeric[A]

  class FromNumeric[A](implicit num: Numeric[A]) extends Ord.FromOrdering[A] with Num[A]{
    override def fromInt(x: Int): A = num.fromInt(x)
    override def plus(x: A, y: A): A = num.plus(x, y)
    override def times(x: A, y: A): A = num.times(x, y)
    override val zero: A = num.zero
    override val one: A = num.one
    override def negate(x: A): A = num.negate(x)
    override def minus(x: A, y: A): A = num.minus(x, y)
  }
}

trait StdNumInstances[TC[typ] >: Num[typ]] extends StdIntegInstances[TC] with StdFracInstances[TC]