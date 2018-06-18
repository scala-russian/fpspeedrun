package fpspeedrun

import fpspeedrun.Ord._
import Eq.ops._
import Semigroup.ops._
import Ord.ops._

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
      x.value match {
        case Left(xx) =>
          y.value match {
            case Left(yy)  => xx <=> yy
            case Right(ys) => xx <=> ys.head
          }
        case Right(xx: List[A]) =>
          y.value match {
            case Left(yy)           => xx.head <=> yy
            case Right(ys: List[A]) => xx <=> ys
          }
    }

  implicit def zipListNum[A](implicit n: Num[A]): Num[ZipList[A]] =
    new Num[ZipList[A]] {
      override def fromInt(x: Int): ZipList[A] =
        ZipList.Repeat(n.fromInt(x))

      override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        x.zipWith(y)(n.plus)

      override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        x.zipWith(y)(n.times)

      override def compare(x: ZipList[A], y: ZipList[A]): Compare =
        x.value match {
          case Left(xx) =>
            y.value match {
              case Left(yy)  => xx <=> yy
              case Right(ys) => xx <=> ys.head
            }
          case Right(xx: List[A]) =>
            y.value match {
              case Left(yy)           => xx.head <=> yy
              case Right(ys: List[A]) => xx <=> ys
            }
        }
    }

  implicit def zipListInteg[A](implicit i: Integ[A]): Integ[ZipList[A]] =
    new Integ[ZipList[A]] {
      override def quotRem(x: ZipList[A],
                           y: ZipList[A]): (ZipList[A], ZipList[A]) =
        (x.zipWith(y)(i.quot), x.zipWith(y)(i.rem))

      override def fromInt(x: Int): ZipList[A] =
        ZipList.Repeat(i.fromInt(x))

      override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        x.zipWith(y)(i.plus)

      override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        x.zipWith(y)(i.times)

      override def compare(x: ZipList[A], y: ZipList[A]): Compare =
        x.value match {
          case Left(xx) =>
            y.value match {
              case Left(yy)  => xx <=> yy
              case Right(ys) => i.compare(xx, ys.head)
            }
          case Right(xs: List[A]) =>
            y.value match {
              case Left(yy)           => i.compare(xs.head, yy)
              case Right(ys: List[A]) => xs <=> ys
            }
        }
    }

  implicit def zipListFrac[A](implicit f: Frac[A]): Frac[ZipList[A]] =
    new Frac[ZipList[A]] {
      override def div(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        x.zipWith(y)(f.div)

      override def fromInt(x: Int): ZipList[A] = ZipList.Repeat(f.fromInt(x))

      override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        x.zipWith(y)(f.plus)

      override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] =
        x.zipWith(y)(f.times)

      override def compare(x: ZipList[A], y: ZipList[A]): Compare =
        x.value match {
          case Left(xx) =>
            y.value match {
              case Left(yy)  => xx <=> yy
              case Right(ys) => xx <=> ys.head
            }
          case Right(xx: List[A]) =>
            y.value match {
              case Left(yy)           => xx.head <=> yy
              case Right(ys: List[A]) => xx <=> ys
            }
        }
    }
}
