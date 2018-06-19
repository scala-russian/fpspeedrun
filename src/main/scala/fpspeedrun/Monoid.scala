package fpspeedrun
import simulacrum.typeclass
import syntax.semigroup._

@typeclass
trait Monoid[A] extends Semigroup[A] with Default[A]{
  def empty: A
  override def default: A = empty
}

sealed trait FreeMonoid[+T]

case object EmptyM extends FreeMonoid[Nothing]
final case class OnlyM[T](x: T) extends FreeMonoid[T]
final case class ConsM[T](x: T, y: FreeMonoid[T]) extends FreeMonoid[T]

object FreeMonoid {

  def apply[T](xs: T*): FreeMonoid[T] = xs match {
    case Seq()     => EmptyM
    case h +: Nil  => OnlyM(h)
    case h +: tail => ConsM(h, apply(tail: _*))
  }

  implicit def freeMonoidMonoid[T]: Monoid[FreeMonoid[T]] = new Monoid[FreeMonoid[T]] {
    def empty: FreeMonoid[T] = EmptyM

    def combine(x: FreeMonoid[T], y: FreeMonoid[T]): FreeMonoid[T] = (x, y) match {
      case (EmptyM, m)                   => m
      case (m, EmptyM)                   => m
      case (OnlyM(a), OnlyM(b))          => ConsM(a, OnlyM(b))
      case (OnlyM(a), c@ConsM(_, _))     => ConsM(a, c)
      case (ConsM(a, as), o@OnlyM(b))    => ConsM(a, combine(as, o))
      case (ConsM(a, as), c@ConsM(_, _)) => ConsM(a, combine(as, c))
    }
  }

  implicit class FreeMonoidOps[T](val x: FreeMonoid[T]) extends AnyVal {
    def reduceAll(implicit mon: Monoid[T]): T = x match {
      case EmptyM       => mon.empty
      case OnlyM(a)     => a
      case ConsM(a, as) => mon.combine(a, as.reduceAll)
    }
  }

}

object Monoid extends StdMonoidInstances[Monoid] {
  implicit def optionMonoid[T: Semigroup]: Monoid[Option[T]] = new Monoid[Option[T]] {
    override def empty: Option[T] = None
    override def combine(xo: Option[T], yo: Option[T]): Option[T] =
      for (x <- xo; y <- yo) yield x |+| y
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
}
