package fpspeedrun
import fpspeedrun.Iso.{Wrapper, WrapperCompanion}
import simulacrum.{op, typeclass}

@typeclass
trait Semigroup[T] {
  @op("|+|", alias = true)
  def combine(x: T, y: T): T
}

object Semigroup extends StdSemigroupInstances

final case class First[T](value: T) extends AnyVal with Wrapper[T]

object First extends WrapperCompanion[First] {
  implicit def firstSemigroup[T]: Semigroup[First[T]] = (x, _) => x
}

final case class Last[T](value: T) extends AnyVal with Wrapper[T]

object Last extends WrapperCompanion[Last] {
  implicit def lastSemigroup[T]: Semigroup[Last[T]] = (_, y) => y
}

trait StdSemigroupInstances extends StdMonoidInstances[Semigroup]
