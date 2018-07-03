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
  type FreeSemigrpoup[T] = NonEmptyList[T]
  implicit val freeConstruct: FreeConstruct[Semigroup, FreeSemigrpoup] =
    new FreeConstruct[Semigroup, FreeSemigrpoup] {
      import syntax.semigroup._
      override def embed[T](x: T): FreeSemigrpoup[T] = NonEmptyList.one(x)
      override def instance[T]: Semigroup[FreeSemigrpoup[T]] = _ ::: _
      override def mapInterpret[A, B](fa: FreeSemigrpoup[A])(f: A => B)(implicit instance: Semigroup[B]): B =
        fa.tail.foldLeft(f(fa.head)){ case (l, r) => l |+| f(r) }
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
