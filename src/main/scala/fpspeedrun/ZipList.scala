package fpspeedrun

import fpspeedrun.Ord.Compare.{GT, LT, EQ}
import syntax.ord._
import syntax.num._

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

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] = (x: ZipList[A], y: ZipList[A]) => (x.value, y.value) match {
    case (Left(a), Left(b)) => a compare b
    case (Left(a), Right(bs)) =>
      if (bs.forall(b => (a compare b) == GT)) GT
      else if (bs.forall(b => (a compare b) == LT)) LT
      else EQ
    case (Right(as), Left(b)) =>
      if (as.forall(a => (a compare b) == GT)) GT
      else if (as.forall(a => (a compare b) == LT)) LT
      else EQ
    case (Right(as), Right(bs)) => as compare bs
  }

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = ???

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = new Integ[ZipList[A]] {
    import syntax.integ._

    def quotRem(x: ZipList[A], y: ZipList[A]): (ZipList[A], ZipList[A]) =
      (x.zipWith(y)((a: A, b: A) => a quot b), x.zipWith(y)((a: A, b: A) => a rem b))

    def fromInt(x: Int): ZipList[A] = ZipList.repeat(implicitly[Integ[A]].fromInt(x))

    def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)((a: A, b: A) => a plus b)

    def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)((a: A, b: A) => a times b)

    def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = (x.value, y.value) match {
      case (Left(a), Left(b)) => a compare b
      case (Left(a), Right(bs)) =>
        if (bs.forall(b => (a compare b) == GT)) GT
        else if (bs.forall(b => (a compare b) == LT)) LT
        else EQ
      case (Right(as), Left(b)) =>
        if (as.forall(a => (a compare b) == GT)) GT
        else if (as.forall(a => (a compare b) == LT)) LT
        else EQ
      case (Right(as), Right(bs)) => as compare bs
    }
  }

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = new Frac[ZipList[A]] {
    import syntax.frac._

    def div(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)((a: A, b: A) => a div b)

    def fromInt(x: Int): ZipList[A] = ZipList.repeat(implicitly[Frac[A]].fromInt(x))

    def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)((a: A, b: A) => a plus b)

    def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)((a: A, b: A) => a times b)

    def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = (x.value, y.value) match {
      case (Left(a), Left(b)) => a compare b
      case (Left(a), Right(bs)) =>
        if (bs.forall(b => (a compare b) == GT)) GT
        else if (bs.forall(b => (a compare b) == LT)) LT
        else EQ
      case (Right(as), Left(b)) =>
        if (as.forall(a => (a compare b) == GT)) GT
        else if (as.forall(a => (a compare b) == LT)) LT
        else EQ
      case (Right(as), Right(bs)) => as compare bs
    }
  }
}
