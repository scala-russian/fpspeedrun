package fpspeedrun
import fpspeedrun.Iso.{Wrapper, WrapperCompanion}
import simulacrum.{op, typeclass}
import syntax.num._

@typeclass
trait Semigroup[T] {
  @op("|+|", alias = true)
  def combine(x: T, y: T): T
}

sealed trait FreeSemigroup[T] {
  val value: List[T]
}

final case class Only[T](x: T) extends FreeSemigroup[T] {
  val value: List[T] = List(x)
}
final case class Concat[T](x: FreeSemigroup[T], y: FreeSemigroup[T]) extends FreeSemigroup[T] {
  val value: List[T] = x.value ++ y.value
}

object FreeSemigroup {

  def apply[T](x: T, y: T*): FreeSemigroup[T] = y match {
    case Seq()     => Only(x)
    case h +: tail => Concat(Only(x), apply[T](h, tail: _*))
  }

  implicit def freeSemigroupSemigroup[T]: Semigroup[FreeSemigroup[T]] =
    (x: FreeSemigroup[T], y: FreeSemigroup[T]) => Concat(x, y)

  implicit class FreeSemigroupOps[T](val x: FreeSemigroup[T]) extends AnyVal {
    def reduceAll(implicit sg: Semigroup[T]): T = x.value.reduce(sg.combine)
  }

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
