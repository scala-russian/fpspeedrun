package fpspeedrun

import fpspeedrun.syntax.eq._
import fpspeedrun.syntax.ord._
import fpspeedrun.syntax.num._
import fpspeedrun.syntax.integ._
import fpspeedrun.syntax.frac._
import fpspeedrun.Ord.Compare._

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

  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] = (x: ZipList[A], y: ZipList[A]) =>
    x.value === y.value

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] = (x: ZipList[A], y: ZipList[A]) => (x, y) match {
    case (Repeat(x1), Repeat(y1)) => x1 <=> y1
    case (Finite(x1), Finite(y1)) => x1 <=> y1
    case (Repeat(x1), Finite(ys)) => ys.collectFirst {
      case y1 if (x1 <=> y1) != EQ => x1 <=> y1
    }.getOrElse(EQ)
    case (Finite(xs), Repeat(y1)) => xs.collectFirst {
      case x1 if (x1 <=> y1) != EQ => x1 <=> y1
    }.getOrElse(EQ)
  }

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = new Num[ZipList[A]] {
    override def fromInt(x: Int): ZipList[A] = Repeat(implicitly[Num[A]].fromInt(x))
    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ + _)
    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ * _)
    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = (x, y) match {
      case (Repeat(x1), Repeat(y1)) => x1 <=> y1
      case (Finite(x1), Finite(y1)) => x1 <=> y1
      case (Repeat(x1), Finite(ys)) => ys.collectFirst {
        case y1 if (x1 <=> y1) != EQ => x1 <=> y1
      }.getOrElse(EQ)
      case (Finite(xs), Repeat(y1)) => xs.collectFirst {
        case x1 if (x1 <=> y1) != EQ => x1 <=> y1
      }.getOrElse(EQ)
    }
  }

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = new Integ[ZipList[A]] {
    override def quotRem(x: ZipList[A], y: ZipList[A]): (ZipList[A], ZipList[A]) = x.zipWith(y)(_ /% _).value match {
      case Left((q, r)) => Repeat(q) -> Repeat(r)
      case Right(qrs) =>
        val a = qrs.unzip
        Finite(a._1) -> Finite(a._2)
    }
    override def fromInt(x: Int): ZipList[A] = Repeat(implicitly[Integ[A]].fromInt(x))
    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ + _)
    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ * _)
    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = (x, y) match {
      case (Repeat(x1), Repeat(y1)) => x1 <=> y1
      case (Finite(x1), Finite(y1)) => x1 <=> y1
      case (Repeat(x1), Finite(ys)) => ys.collectFirst {
        case y1 if (x1 <=> y1) != EQ => x1 <=> y1
      }.getOrElse(EQ)
      case (Finite(xs), Repeat(y1)) => xs.collectFirst {
        case x1 if (x1 <=> y1) != EQ => x1 <=> y1
      }.getOrElse(EQ)
    }
  }

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = new Frac[ZipList[A]] {
    override def div(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ / _)
    override def fromInt(x: Int): ZipList[A] = Repeat(implicitly[Frac[A]].fromInt(x))
    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ + _)
    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ * _)
    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = (x, y) match {
      case (Repeat(x1), Repeat(y1)) => x1 <=> y1
      case (Finite(x1), Finite(y1)) => x1 <=> y1
      case (Repeat(x1), Finite(ys)) => ys.collectFirst {
        case y1 if (x1 <=> y1) != EQ => x1 <=> y1
      }.getOrElse(EQ)
      case (Finite(xs), Repeat(y1)) => xs.collectFirst {
        case x1 if (x1 <=> y1) != EQ => x1 <=> y1
      }.getOrElse(EQ)
    }
  }
}
