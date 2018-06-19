package fpspeedrun
import simulacrum.typeclass
import syntax.semigroup._

@typeclass
trait Monoid[A] extends Semigroup[A] with Default[A]{
  def empty: A
  override def default: A = empty
}

sealed trait FreeMonoid[+T] {
  val value: List[T]
}

case object EmptyM extends FreeMonoid[Nothing] {
  val value: List[Nothing] = List()
}
final case class OnlyM[T](x: T) extends FreeMonoid[T] {
  val value: List[T] = List(x)
}
final case class ConcatM[T](x: FreeMonoid[T], y: FreeMonoid[T]) extends FreeMonoid[T] {
  val value: List[T] = x.value ++ y.value
}

object FreeMonoid {

  def apply[T](xs: T*): FreeMonoid[T] = xs match {
    case Seq()     => EmptyM
    case h +: Nil  => OnlyM(h)
    case h +: tail => ConcatM(OnlyM(h), apply(tail: _*))
  }

  implicit def freeMonoidMonoid[T]: Monoid[FreeMonoid[T]] = new Monoid[FreeMonoid[T]] {
    def empty: FreeMonoid[T] = EmptyM

    def combine(x: FreeMonoid[T], y: FreeMonoid[T]) = ConcatM(x, y)
  }

  implicit class FreeMonoidOps[T](val x: FreeMonoid[T]) extends AnyVal {
    def reduceAll(implicit mon: Monoid[T]): T = x.value.fold(mon.empty)(mon.combine)
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
