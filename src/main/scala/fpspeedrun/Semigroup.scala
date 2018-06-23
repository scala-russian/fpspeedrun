package fpspeedrun
import cats.data.NonEmptyList
import fpspeedrun.Iso.{Wrapper, WrapperCompanion}
import simulacrum.{op, typeclass}

@typeclass
trait Semigroup[T] extends Magma[T] {
  @op("|+|", alias = true)
  def combine(x: T, y: T): T

  override def merge(x: T, y: T): T = combine(x, y)
}

object Semigroup extends StdSemigroupInstances[Semigroup]{
  type FreeSemigroup[T] = NonEmptyList[T]
  implicit val freeConstruct: FreeConstruct[Semigroup, FreeSemigroup] =
    new FreeConstruct[Semigroup, FreeSemigroup] {
      override def embed[T](x: T): FreeSemigroup[T] = NonEmptyList.one(x)
      override def instance[T]: Semigroup[FreeSemigroup[T]] = (x: FreeSemigroup[T], y: FreeSemigroup[T]) => x ::: y
      override def mapInterpret[A, B](fa: FreeSemigroup[A])(f: A => B)(implicit instance: Semigroup[B]): B =
        fa.map(f).reduce(instance.combine)
    }

}

final case class First[T](value: T) extends AnyVal with Wrapper[T]

object First extends WrapperCompanion[First] {
  implicit def firstSemigroup[T]: Semigroup[First[T]] = (x, _) => x
}

final case class Last[T](value: T) extends AnyVal with Wrapper[T]

object Last extends WrapperCompanion[Last] {
  implicit def lastSemigroup[T]: Semigroup[Last[T]] = (_, y) => y
}

trait StdSemigroupInstances[TC[x] >: Semigroup[x]]
    extends StdMonoidInstances[TC]
