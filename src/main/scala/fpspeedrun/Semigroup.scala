package fpspeedrun
import fpspeedrun.Iso.{Wrapper, WrapperIso}
import simulacrum.{op, typeclass}
import syntax.num._


@typeclass
trait Semigroup[T] {
  @op("|+|", alias = true)
  def combine(x: T, y: T): T
}

object Semigroup extends StdSemigroupInstances


final case class Sum[T](value: T) extends AnyVal with Wrapper[T]

object Sum{
  implicit def sumSemigroup[T: Num]: Semigroup[Sum[T]] = (x, y) => Sum(x.value + y.value)
  implicit def iso[A]: WrapperIso[A, Sum] = Sum(_)
}

final case class Prod[T](value: T) extends AnyVal with Wrapper[T]

object Prod {
  implicit def prodSemigroup[T: Num]: Semigroup[Prod[T]] = (x, y) => Prod(x.value * y.value)
  implicit def iso[A]: WrapperIso[A, Prod] = Prod(_)
}

final case class First[T](value: T) extends AnyVal with Wrapper[T]

object First {
  implicit def firstSemigroup[T]: Semigroup[First[T]] = (x, _) => x
  implicit def iso[A]: WrapperIso[A, First] = First(_)
}

final case class Last[T](value: T) extends AnyVal with Wrapper[T]

object Last {
  implicit def lastSemigroup[T]: Semigroup[Last[T]] = (_, y) => y
  implicit def iso[A]: WrapperIso[A, Last] = Last(_)
}

trait StdSemigroupInstances extends StdMonoidInstances[Semigroup]
