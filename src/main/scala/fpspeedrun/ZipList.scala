package fpspeedrun

import fpspeedrun.Ord.Compare

import syntax.semigroup._
import syntax.eq._
import syntax.ord._
import syntax.num._
import syntax.integ._
import syntax.frac._

trait ZipList[A] {
  def value: Either[A, List[A]]

  def zipWith[B, C](that: ZipList[B])(f: (A, B) => C): ZipList[C]
}

object ZipList {
  final case class Repeat[A](single: A) extends ZipList[A] {
    override def value = Left(single)
    override def zipWith[B, C](that: ZipList[B])(f: (A, B) => C): ZipList[C] =
      that match {
        case Repeat(b)  => Repeat(f(single, b))
        case Finite(xs) => Finite(xs.map(f(single, _)))
      }
  }
  final case class Finite[A](list: List[A]) extends ZipList[A] {
    override def value = Right(list)
    override def zipWith[B, C](that: ZipList[B])(f: (A, B) => C): ZipList[C] =
      that match {
        case Repeat(b)  => Finite(list.map(f(_, b)))
        case Finite(ys) => Finite((list, ys).zipped.map(f))
      }
  }

  def apply[A](list: List[A]): ZipList[A] = Finite(list)
  def repeat[A](value: A): ZipList[A] = Repeat(value)

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] =
    (x: ZipList[A], y: ZipList[A]) => x.zipWith(y)(_ |+| _)

  implicit def zipListMonoid[A](implicit m: Monoid[A]): Monoid[ZipList[A]] =
    new Monoid[ZipList[A]] {
      override def empty: ZipList[A] = ZipList.Repeat(m.empty)
      override def combine(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        x.zipWith(y)(_ |+| _)
    }

  implicit def zipListEq[A](implicit eq1: Eq[A]): Eq[ZipList[A]] =
    (x: ZipList[A], y: ZipList[A]) => x.value === y.value

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] =
    (x: ZipList[A], y: ZipList[A]) =>
      (x.value, y.value) match {
        case (Left(xx), Left(yy))   => xx <=> yy
        case (Left(xx), Right(ys))  => xx <=> ys.head
        case (Right(xs), Left(yy))  => xs.head <=> yy
        case (Right(xs), Right(ys)) => xs <=> ys
    }

  implicit def zipListNum[A: Num]: Num[ZipList[A]] =
    new Num[ZipList[A]] {
      override def fromInt(x: Int): ZipList[A] =
        ZipList.Repeat(x.toNum)

      override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        x.zipWith(y)(_ + _)

      override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        x.zipWith(y)(_ * _)

      override def compare(x: ZipList[A], y: ZipList[A]): Compare =
        zipListOrd(Ord[A]).compare(x, y)
    }

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] =
    new Integ[ZipList[A]] {
      override def quotRem(x: ZipList[A],
                           y: ZipList[A]): (ZipList[A], ZipList[A]) =
        (x.zipWith(y)(_ / _), x.zipWith(y)(_ % _))

      override def fromInt(x: Int): ZipList[A] =
        zipListNum(Num[A]).fromInt(x)

      override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        zipListNum(Num[A]).plus(x, y)

      override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        zipListNum(Num[A]).times(x, y)

      override def compare(x: ZipList[A], y: ZipList[A]): Compare =
        zipListOrd(Ord[A]).compare(x, y)
    }

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] =
    new Frac[ZipList[A]] {
      override def div(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        x.zipWith(y)(_ / _)

      override def fromInt(x: Int): ZipList[A] =
        zipListNum(Num[A]).fromInt(x)

      override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        zipListNum(Num[A]).plus(x, y)

      override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        zipListNum(Num[A]).times(x, y)

      override def compare(x: ZipList[A], y: ZipList[A]): Compare =
        zipListOrd(Ord[A]).compare(x, y)
    }
}
