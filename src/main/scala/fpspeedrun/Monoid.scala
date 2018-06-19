package fpspeedrun
import fpspeedrun.Iso.{ Wrapper, WrapperCompanion }
import simulacrum.typeclass
import syntax.semigroup._

@typeclass
trait Monoid[A] extends Semigroup[A] with Default[A]{
  def empty: A
  override def default: A = empty
}

object Monoid extends StdMonoidInstances[Monoid] {
  implicit def optionMonoid[T: Semigroup]: Monoid[Option[T]] = new Monoid[Option[T]] {
    override def empty: Option[T] = None
    override def combine(xo: Option[T], yo: Option[T]): Option[T] =
      for (x <- xo; y <- yo) yield x |+| y
  }
}


final case class Endo[A](run: A => A) extends AnyVal

object Endo{
  implicit def endoMonoid[A]: Monoid[Endo[A]] = new Monoid[Endo[A]] {
    override def empty: Endo[A] = Endo(identity)
    override def combine(x: Endo[A], y: Endo[A]): Endo[A] = Endo(x.run andThen y.run)
  }
}

final case class Sum[T](value: T) extends AnyVal with Wrapper[T]

object Sum extends WrapperCompanion[Sum] {
  import syntax.num._
  implicit def sumMonoid[T: Num]: Monoid[Sum[T]] = new Monoid[Sum[T]] {
    override def empty: Sum[T] = Sum(zero)
    override def combine(x: Sum[T], y: Sum[T]): Sum[T] = Sum(x.value + y.value)
  }
}

final case class Prod[T](value: T) extends AnyVal with Wrapper[T]

object Prod extends WrapperCompanion[Prod] {
  import syntax.num._
  implicit def prodMonoid[T: Num]: Monoid[Prod[T]] = new Monoid[Prod[T]] {
    override def empty: Prod[T] = Prod(one)
    override def combine(x: Prod[T], y: Prod[T]): Prod[T] = Prod(x.value * y.value)
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

  final implicit def vectorMonoid[A]: TC[Vector[A]] = new Monoid[Vector[A]] {
    override def empty: Vector[A] = Vector.empty
    override def combine(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
  }
}
