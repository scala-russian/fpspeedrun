package fpspeedrun

import fpspeedrun.Eq._
import fpspeedrun.syntax.eq._
import fpspeedrun.syntax.frac._
import fpspeedrun.syntax.integ._
import fpspeedrun.syntax.num._
import fpspeedrun.syntax.ord._
import fpspeedrun.syntax.semigroup._

trait ZipList[A] {
  def value: Either[A, List[A]]

  def zipWith[B, C](that: ZipList[B])(f: (A, B) => C): ZipList[C]
}

object ZipList {

  final case class Repeat[A](single: A) extends ZipList[A] {
    override def value = Left(single)

    override def zipWith[B, C](that: ZipList[B])(f: (A, B) => C): ZipList[C] = that match {
      case Repeat(b) => Repeat(f(single, b))
      case Finite(xs) => Finite(xs.map(f(single, _)))
    }
  }

  final case class Finite[A](list: List[A]) extends ZipList[A] {
    override def value = Right(list)

    override def zipWith[B, C](that: ZipList[B])(f: (A, B) => C): ZipList[C] = that match {
      case Repeat(b) => Finite(list.map(f(_, b)))
      case Finite(ys) => Finite((list, ys).zipped.map(f))
    }
  }

  def apply[A](list: List[A]): ZipList[A] = Finite(list)

  def repeat[A](value: A): ZipList[A] = Repeat(value)

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] =
    (x: ZipList[A], y: ZipList[A]) => x.zipWith(y)(_ |+| _)

  implicit def zipListMonoid[A: Monoid]: Monoid[ZipList[A]] = new Monoid[ZipList[A]] {
    override def empty: ZipList[A] = repeat(Monoid[A].empty)

    override def combine(x: ZipList[A], y: ZipList[A]): ZipList[A] =
      x.zipWith(y)(_ |+| _)
  }

  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] =
    (x: ZipList[A], y: ZipList[A]) => x.value === y.value

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] =
    (x: ZipList[A], y: ZipList[A]) => (x.value, y.value) match {
      case (Right(xo), Right(yo)) => xo <=> yo
      case (Left(xo), Right(yo)) => List.fill(yo.length)(xo) <=> yo
      case (Right(xo), Left(yo)) => xo <=> List.fill(xo.length)(yo)
      case (Left(xo), Left(yo)) => xo <=> yo
    }

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = new Num[ZipList[A]] {
    override def fromInt(x: Int): ZipList[A] = Repeat(x.toNum)

    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ + _)

    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ * _)

    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare =
      (x.value, y.value) match {
        case (Right(xo), Right(yo)) => xo <=> yo
        case (Left(xo), Right(yo)) => List.fill(yo.length)(xo) <=> yo
        case (Right(xo), Left(yo)) => xo <=> List.fill(xo.length)(yo)
        case (Left(xo), Left(yo)) => xo <=> yo
      }

  }

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = new Integ[ZipList[A]] {
    override def quotRem(x: ZipList[A], y: ZipList[A]): (ZipList[A], ZipList[A]) =
      x.zipWith(y)(_ quotRem _).value match {
        case Left((q, r)) => Repeat(q) -> Repeat(r)
        case Right(qrs) => qrs.unzip match {
          case (l, r) => ZipList(l) -> ZipList(r)
        }
      }

    override def fromInt(x: Int): ZipList[A] = Repeat(x.toNum)

    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ plus _)

    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ times _)

    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare =
      (x.value, y.value) match {
        case (Right(xo), Right(yo)) => xo <=> yo
        case (Left(xo), Right(yo)) => List.fill(yo.length)(xo) <=> yo
        case (Right(xo), Left(yo)) => xo <=> List.fill(xo.length)(yo)
        case (Left(xo), Left(yo)) => xo <=> yo
      }
  }

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = new Frac[ZipList[A]] {
    override def div(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ / _)

    override def fromInt(x: Int): ZipList[A] = fromInt(x)

    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ plus _)

    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ times _)

    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare =
      (x.value, y.value) match {
        case (Right(xo), Right(yo)) => xo <=> yo
        case (Left(xo), Right(yo)) => List.fill(yo.length)(xo) <=> yo
        case (Right(xo), Left(yo)) => xo <=> List.fill(xo.length)(yo)
        case (Left(xo), Left(yo)) => xo <=> yo
      }
  }
}
