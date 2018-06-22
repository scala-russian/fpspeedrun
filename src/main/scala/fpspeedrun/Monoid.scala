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

  type FreeMonoid[A] = List[A]

  implicit val freeMonoid: FreeConstruct[Monoid, FreeMonoid] =
    new FreeConstruct[Monoid, FreeMonoid] {
      override def embed[T](x: T): FreeMonoid[T] = List(x)
      override def instance[T]: Monoid[FreeMonoid[T]] = new Monoid[FreeMonoid[T]] {
        override def empty: FreeMonoid[T] = List.empty

        override def combine(x: FreeMonoid[T], y: FreeMonoid[T]): FreeMonoid[T] = x ++ y
      }
      override def mapInterpret[A, B](fa: FreeMonoid[A])(f: A => B)(implicit instance: Monoid[B]): B =
        fa.map(f).fold(instance.empty)(instance.combine)
    }
}

final case class Endo[A](run: A => A) extends AnyVal

object Endo {
  implicit def endoMonoid[A]: Monoid[Endo[A]] = new Monoid[Endo[A]] {
    def empty: Endo[A] = Endo[A]((x: A) => x)
    def combine(x: Endo[A], y: Endo[A]): Endo[A] = Endo[A](x.run andThen y.run)
  }
}

final case class Sum[T](value: T) extends AnyVal with Wrapper[T]

object Sum extends WrapperCompanion[Sum] {
  implicit def sumMonoid[T: Num]: Monoid[Sum[T]] = new Monoid[Sum[T]] {
    def empty: Sum[T] = wrapperIso[T].wrap(implicitly[Num[T]].fromInt(0))
    def combine(x: Sum[T], y: Sum[T]): Sum[T] =
      wrapperIso[T].wrap(implicitly[Num[T]].plus(wrapperIso[T].unwrap(x), wrapperIso[T].unwrap(y)))
  }
}

final case class Prod[T](value: T) extends AnyVal with Wrapper[T]

object Prod extends WrapperCompanion[Prod] {
  implicit def prodMonoid[T: Num]: Monoid[Prod[T]] = new Monoid[Prod[T]] {
    def empty: Prod[T] = wrapperIso[T].wrap(implicitly[Num[T]].fromInt(1))
    def combine(x: Prod[T], y: Prod[T]): Prod[T] =
      wrapperIso[T].wrap(implicitly[Num[T]].times(wrapperIso[T].unwrap(x), wrapperIso[T].unwrap(y)))
  }
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
