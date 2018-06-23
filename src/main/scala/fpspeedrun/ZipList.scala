package fpspeedrun

import syntax.integ._
import syntax.num._
import syntax.ord._
import syntax.frac._

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

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] = ???

  implicit def zipListMonoid[A: Monoid]: Monoid[ZipList[A]] = ???

  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] = ???

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] =
    (x: ZipList[A], y: ZipList[A]) =>
      (x, y) match {
        case (Repeat(a), Repeat(b)) => a <=> b
        case (Finite(xs), Repeat(b)) => xs <=> List.fill(xs.size)(b)
        case (Repeat(a), Finite(xs)) => List.fill(xs.size)(a) <=> xs
        case (Finite(xs), Finite(ys)) => xs <=> ys
      }

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = ???

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = new Integ[ZipList[A]] {
    override def quotRem(x: ZipList[A], y: ZipList[A]): (ZipList[A], ZipList[A]) =
      (x.zipWith(y)(_ / _), x.zipWith(y)(_ % _))
    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ * _)
    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ + _)
    override def fromInt(x: Int): ZipList[A] = ???
    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = (x, y) match {
      case (Repeat(a), Repeat(b)) => a <=> b
      case (Finite(xs), Repeat(b)) => xs <=> List.fill(xs.size)(b)
      case (Repeat(a), Finite(xs)) => List.fill(xs.size)(a) <=> xs
      case (Finite(xs), Finite(ys)) => xs <=> ys
    }
  }

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = new Frac[ZipList[A]] {
    override def div(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ / _)
    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = ???
    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = ???
    override def fromInt(x: Int): ZipList[A] = ???
    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = (x, y) match {
      case (Repeat(a), Repeat(b)) => a <=> b
      case (Finite(xs), Repeat(b)) => xs <=> List.fill(xs.size)(b)
      case (Repeat(a), Finite(xs)) => List.fill(xs.size)(a) <=> xs
      case (Finite(xs), Finite(ys)) => xs <=> ys
    }
  }
}
