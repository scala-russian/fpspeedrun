package fpspeedrun.syntax
import fpspeedrun._

object eq extends Eq.ToEqOps

object ord extends Ord.ToOrdOps

object num extends Num.ToNumOps {
  def zero[T: Num]: T = Num[T].zero
  def one[T: Num]: T = Num[T].one
  implicit class IntNumOps(val x: Int) extends AnyVal {
    def toNum[T](implicit num: Num[T]): T = num.fromInt(x)
  }
}

object integ extends Integ.ToIntegOps

object frac extends Frac.ToFracOps

object ratio {
  implicit class RatioOps[T](val x: T) extends AnyVal {
    def \\(y: T)(implicit int: Integ[T]): Ratio[T] = Ratio.make(x, y)
    def toRatio(implicit int: Integ[T]): Ratio[T] = Ratio.make(x, int.one)
  }
}

object semigroup extends Semigroup.ToSemigroupOps {
  implicit class ListOps[A](val xs: List[A]) extends AnyVal {
    def reduceOpt(implicit sg: Semigroup[A]): Option[A] =
      xs.reduceOption(sg.combine)

    def reduceMapOpt[B](f: A => B)(implicit sg: Semigroup[B]): Option[B] =
      xs match {
        case Nil       => None
        case x :: rest => Some(rest.foldLeft(f(x))((b, a) => b |+| f(a)))
      }

    def reduceOptVia[F[_]](implicit iso: Iso[A, F[A]],
                           sg: Semigroup[F[A]]): Option[A] =
      xs.reduceOption((x, y) => iso.unwrap(iso.wrap(x) |+| iso.wrap(y)))
  }

  implicit class SemigroupNewtypeOps[T](val x: T) extends AnyVal {
    def sum: Sum[T] = Sum(x)
    def prod: Prod[T] = Prod(x)
    def first: First[T] = First(x)
    def last: Last[T] = Last(x)
  }
}

object monoid extends Monoid.ToMonoidOps {
  def empty[T](implicit ev$1: Monoid[T]): T = ev$1.empty

  implicit class ListOps[A](val xs: List[A]) extends AnyVal {
    def foldAll(implicit mon: Monoid[A]): A =
      xs.foldLeft(mon.empty)(mon.combine)

    def foldMap[B](f: A => B)(implicit m: Monoid[B]): B =
      xs.map(f).foldLeft(m.empty)(m.combine)

    def foldVia[F[_]](implicit iso: Iso[A, F[A]], mon: Monoid[F[A]]): A =
      xs.foldLeft(iso.unwrap(mon.empty))((a, b) =>
        iso.unwrap(mon.combine(iso.wrap(a), iso.wrap(b))))
  }
}
