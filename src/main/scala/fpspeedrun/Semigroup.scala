package fpspeedrun
import fpspeedrun.Iso.{Wrapper, WrapperCompanion}
import simulacrum.{op, typeclass}
import syntax.num._

@typeclass
trait Semigroup[T] {
  @op("|+|", alias = true)
  def combine(x: T, y: T): T
}

object Semigroup extends StdSemigroupInstances

final case class Sum[T](value: T) extends AnyVal with Wrapper[T]

object Sum extends WrapperCompanion[Sum] {
  implicit def sumSemigroup[T: Num]: Semigroup[Sum[T]] = (x, y) => Sum(x.value + y.value)
}

final case class Prod[T](value: T) extends AnyVal with Wrapper[T]

object Prod extends WrapperCompanion[Prod] {
  implicit def prodSemigroup[T: Num]: Semigroup[Prod[T]] = (x, y) => Prod(x.value * y.value)
}

final case class First[T](value: T) extends AnyVal with Wrapper[T]

object First extends WrapperCompanion[First] {
  implicit def firstSemigroup[T]: Semigroup[First[T]] = (x, _) => x
}

final case class Last[T](value: T) extends AnyVal with Wrapper[T]

object Last extends WrapperCompanion[Last] {
  implicit def lastSemigroup[T]: Semigroup[Last[T]] = (_, y) => y
}

trait StdSemigroupInstances extends StdMonoidInstances[Semigroup]
