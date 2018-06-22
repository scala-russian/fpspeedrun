package fpspeedrun.syntax
import fpspeedrun._

import scala.annotation.tailrec

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
        case Nil => None
        case x :: rest => Some(rest.foldLeft(f(x))((b, a) => b |+| f(a)))
      }

    def reduceOptVia[F[_]](implicit iso: Iso[A, F[A]], sg: Semigroup[F[A]]): Option[A] =
      xs.reduceOption((x, y) => iso.unwrap(iso.wrap(x) |+| iso.wrap(y)))
  }

  implicit class SemigroupNewtypeOps[T](val x: T) extends AnyVal {
    def sum: Sum[T] = Sum(x)
    def prod: Prod[T] = Prod(x)
    def first: First[T] = First(x)
    def last: Last[T] = Last(x)
  }

  implicit class FreeMagmaOps[T](val x: FreeMagma[T]) extends AnyVal {
    @tailrec final def flat(x: List[FreeMagma[T]]): List[Leaf[T]] = {
      val list: List[FreeMagma[T]] = x.flatMap {
        case l@Leaf(_) => List(l)
        case Branch(l, r) => List(l, r)
      }

      if (x.size == list.size) x.map(_.asInstanceOf[Leaf[T]])
      else flat(list)
    }

    def reduceAll(implicit sg: Semigroup[T]): T = x match {
      case Leaf(a) => a
      case b@Branch(_, _) => flat(List(b)) map { case Leaf(l) => l } reduce sg.combine
    }
  }
}

object monoid extends Monoid.ToMonoidOps
