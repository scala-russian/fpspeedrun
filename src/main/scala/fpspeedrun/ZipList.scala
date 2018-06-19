package fpspeedrun

import fpspeedrun.syntax.eq._
import fpspeedrun.syntax.ord._
import fpspeedrun.syntax.num._
import fpspeedrun.syntax.integ._
import fpspeedrun.syntax.frac._
import fpspeedrun.syntax.semigroup._
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

  private class ZipListSemigroup[A: Semigroup] extends Semigroup[ZipList[A]] {
    override def combine(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ combine _)
  }

  private class ZipListMonoid[A: Monoid] extends ZipListSemigroup[A] with Monoid[ZipList[A]] {
    override def empty: ZipList[A] = repeat(Monoid[A].empty)
  }

  private class ZipListEq[A: Eq] extends Eq[ZipList[A]] {
    override def equal(x: ZipList[A], y: ZipList[A]): Boolean = x.value === y.value
  }

  private class ZipListOrd[A: Ord] extends ZipListEq[A] with Ord[ZipList[A]] {
    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = (x.value, y.value) match {
      case (Left(x1), Left(y1)) => x1 <=> y1
      case (Right(x1), Right(y1)) => x1 <=> y1
      case (Left(x1), Right(ys)) => ys.collectFirst {
        case y1 if (x1 <=> y1) != EQ => x1 <=> y1
      }.getOrElse(EQ)
      case (Right(xs), Left(y1)) => xs.collectFirst {
        case x1 if (x1 <=> y1) != EQ => x1 <=> y1
      }.getOrElse(EQ)
    }
  }

  private class ZipListNum[A: Num] extends ZipListOrd[A] with Num[ZipList[A]] {
    override def fromInt(x: Int): ZipList[A] = Repeat(x.toNum)
    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ + _)
    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ * _)
  }

  private class ZipListInteg[A: Integ] extends ZipListNum[A] with Integ[ZipList[A]] {
    override def quotRem(x: ZipList[A], y: ZipList[A]): (ZipList[A], ZipList[A]) = x.zipWith(y)(_ /% _).value match {
      case Left((q, r)) => Repeat(q) -> Repeat(r)
      case Right(qrs) => qrs.unzip match {
        case (l, r) => ZipList(l) -> ZipList(r)
      }
    }
  }

  private class ZipListFrac[A: Frac] extends ZipListNum[A] with Frac[ZipList[A]] {
    override def div(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ / _)
  }

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] = new ZipListSemigroup[A]
  implicit def zipListMonoid[A: Monoid]: Monoid[ZipList[A]] = new ZipListMonoid[A]
  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] = new ZipListEq[A]
  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] = new ZipListOrd[A]
  implicit def zipListNum[A: Num]: Num[ZipList[A]] = new ZipListNum[A]
  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = new ZipListInteg[A]
  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = new ZipListFrac[A]
}
