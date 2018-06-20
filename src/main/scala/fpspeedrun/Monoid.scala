package fpspeedrun
import fpspeedrun.Iso.{Wrapper, WrapperCompanion}
import simulacrum.typeclass
import syntax.semigroup._

@typeclass
trait Monoid[A] extends Semigroup[A] with Default[A] {
  def empty: A
  override def default: A = empty
}

object Monoid extends StdMonoidInstances[Monoid]{
  implicit def optionMonoid[T: Semigroup]: Monoid[Option[T]] =
    new Monoid[Option[T]] {
      override def empty: Option[T] = None
      override def combine(xo: Option[T], yo: Option[T]): Option[T] =
        for (x <- xo; y <- yo) yield x |+| y
    }

  //TODO find the real type. Hint: look below
  type FreeMonoid[A] = Nothing

  implicit val freeMonoid: FreeConstruct[Monoid, FreeMonoid] =
    new FreeConstruct[Monoid, FreeMonoid] {
      override def embed[T](x: T): FreeMonoid[T] = ???
      override def instance[T]: Monoid[FreeMonoid[T]] = ???
      override def mapInterpret[A, B](fa: FreeMonoid[A])(f: A => B)(implicit instance: Monoid[B]): B = ???
    }
}

final case class Endo[A](run: A => A) extends AnyVal

object Endo {
  implicit def endoMonoid[A]: Monoid[Endo[A]] = ???
}

final case class Sum[T](value: T) extends AnyVal with Wrapper[T]

object Sum extends WrapperCompanion[Sum] {
  implicit def sumMonoid[T: Num]: Monoid[Sum[T]] = ???
}

final case class Prod[T](value: T) extends AnyVal with Wrapper[T]

object Prod extends WrapperCompanion[Prod] {
  implicit def prodMonoid[T: Num]: Monoid[Prod[T]] = ???
}

trait StdMonoidInstances[TC[x] >: Monoid[x]] {
  final implicit val stringMonoid: TC[String] = new Monoid[String] {
    override def empty: String = ""
    override def combine(x: String, y: String): String = x + y
  }

  final implicit def listMonoid[A]: TC[List[A]] = new Monoid[List[A]] {
    override def empty: List[A] = List.empty
    override def combine(x: List[A], y: List[A]): List[A] = x ::: y
  }

  final implicit def vectorMonoid[A]: TC[Vector[A]] = ???
}
