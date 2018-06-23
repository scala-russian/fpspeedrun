package fpspeedrun
import fpspeedrun.syntax.monoid
import fpspeedrun.syntax.semigroup._
import fpspeedrun.syntax.eq._
import fpspeedrun.syntax.ord._

sealed trait ZipList[A] {
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

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] = (x: ZipList[A], y: ZipList[A]) => x.zipWith(y)(_ |+| _)

  implicit def zipListMonoid[A: Monoid]: Monoid[ZipList[A]] = new Monoid[ZipList[A]] {

    override def empty: ZipList[A] = repeat(monoid.empty[A])

    override def combine(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(_ |+| _)
  }

  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] = (x: ZipList[A], y: ZipList[A]) => (x, y) match {
    case (Repeat(r1), Repeat(r2)) => r1 === r2
    case (Finite(f1), Finite(f2)) => Eq.listEq[A].equal(f1, f2)
    case _ => false
  }

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] = (x: ZipList[A], y: ZipList[A]) => (x, y) match {
    case (Repeat(r1), Repeat(r2)) => r1 <=> r2
    case (Finite(f1), Finite(f2)) => Ord.compareLists(f1, f2)
    case (Repeat(r), Finite(f)) => Option(r) <=> f.headOption
    case (Finite(f), Repeat(r)) => f.headOption <=> Option(r)
  }

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = new NumZipList[A]

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = new IntegZipList[A]

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = new FracZipList[A]


  private class NumZipList[A: Num] extends Num[ZipList[A]] {

    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(Num[A].times)

    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(Num[A].plus)

    override def fromInt(x: Int): ZipList[A] = repeat(Num[A].fromInt(x))

    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = zipListOrd[A].compare(x, y)
  }

  private class IntegZipList[A: Integ] extends NumZipList[A] with Integ[ZipList[A]]{
    override def quotRem(x: ZipList[A], y: ZipList[A]): (ZipList[A], ZipList[A]) = x.zipWith(y)(Integ[A].quotRem) match {
      case Repeat(s) => (Repeat(s._1), Repeat(s._2))
      case Finite(l) => (Finite(l.map(_._1)), Finite(l.map(_._2)))
    }
  }

  private class FracZipList[A: Frac] extends NumZipList[A] with Frac[ZipList[A]] {
    override def div(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(Frac[A].div)
  }

}
