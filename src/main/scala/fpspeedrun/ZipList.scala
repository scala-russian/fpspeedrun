package fpspeedrun
import cats.{Eq, Monoid, Order, Semigroup}
import syntax.num._
import syntax.integ._
import syntax.frac._
import cats.syntax.order._
import cats.instances.int._
import cats.syntax.semigroup._
import cats.instances.either._
import cats.instances.list._

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
  def repeat[A](value: A): ZipList[A]     = Repeat(value)

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] =
    new ZipListSemigroup[A]

  implicit def zipListMonoid[A: Monoid]: Monoid[ZipList[A]] =
    new ZipListMonoid[A]

  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] = _.value === _.value

  implicit def zipListOrd[A: Order]: Order[ZipList[A]] = new ZipListOrd[A]

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = new ZipListNum[A]

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = new ZipListInteg[A]

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = new ZipListFrac[A]

  class ZipListOrd[A: Order] extends Order[ZipList[A]] {
    override def compare(xs: ZipList[A], ys: ZipList[A]): Int =
      (xs, ys) match {
        case (Repeat(x), Repeat(y)) => x compare y
        case (Repeat(x), Finite(y)) =>
          y.view.map(x compare _).find(_ =!= 0).getOrElse(0)
        case (Finite(x), Repeat(y)) =>
          x.view.map(_ compare y).find(_ =!= 0).getOrElse(0)
        case (Finite(x), Finite(y)) => x compare y
      }
  }

  class ZipListNum[A: Num] extends ZipListOrd[A] with Num[ZipList[A]] {
    override def fromInt(x: Int): ZipList[A] = repeat(x.toNum[A])
    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] =
      x.zipWith(y)(_ + _)
    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] =
      x.zipWith(y)(_ * _)
  }

  class ZipListInteg[A: Integ] extends ZipListNum[A] with Integ[ZipList[A]] {
    override def quotRem(xs: ZipList[A], ys: ZipList[A]): (ZipList[A], ZipList[A]) =
      xs.zipWith(ys)(_ /% _) match {
        case Repeat((q, r)) => (Repeat(q), Repeat(r))
        case Finite(pairs) =>
          val (qs, rs) = pairs.unzip
          (Finite(qs), Finite(rs))
      }
  }

  class ZipListFrac[A: Frac] extends ZipListNum[A] with Frac[ZipList[A]] {
    override def div(x: ZipList[A], y: ZipList[A]): ZipList[A] =
      x.zipWith(y)(_ / _)
  }

  class ZipListSemigroup[A: Semigroup] extends Semigroup[ZipList[A]] {
    override def combine(x: ZipList[A], y: ZipList[A]): ZipList[A] =
      x.zipWith(y)(_ |+| _)
  }

  class ZipListMonoid[A: Monoid] extends ZipListSemigroup[A] with Monoid[ZipList[A]] {
    override def empty: ZipList[A] = repeat(Monoid[A].empty)
  }

}
