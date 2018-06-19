package fpspeedrun
import fpspeedrun.Iso.{Wrapper, WrapperCompanion}
import simulacrum.{op, typeclass}
import syntax.num._

@typeclass
trait Semigroup[T] {
  @op("|+|", alias = true)
  def combine(x: T, y: T): T
}

sealed trait FreeSemigroup[T]

final case class Only[T](x: T) extends FreeSemigroup[T]
final case class Cons[T](x: T, y: FreeSemigroup[T]) extends FreeSemigroup[T]

object FreeSemigroup {

  def apply[T](x: T, y: T*): FreeSemigroup[T] = y match {
    case Seq()     => Only(x)
    case h +: tail => Cons(x, apply[T](h, tail: _*))
  }

  implicit def freeSemigroupSemigroup[T]: Semigroup[FreeSemigroup[T]] = new Semigroup[FreeSemigroup[T]] {
    def combine(x: FreeSemigroup[T], y: FreeSemigroup[T]): FreeSemigroup[T] = (x, y) match {
      case (Only(a), Only(b))          => Cons(a, Only(b))
      case (Only(a), c@Cons(_, _))     => Cons(a, c)
      case (Cons(a, as), o@Only(b))    => Cons(a, combine(as, o))
      case (Cons(a, as), c@Cons(_, _)) => Cons(a, combine(as, c))
    }
  }

  implicit class FreeSemigroupOps[T](val x: FreeSemigroup[T]) extends AnyVal {
    def reduceAll(implicit sg: Semigroup[T]): T = x match {
      case Only(a) => a
      case Cons(a, as) => sg.combine(a, as.reduceAll)
    }
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
