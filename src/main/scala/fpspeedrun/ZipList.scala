package fpspeedrun


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

  private def ordZl[A: Ord](x: ZipList[A], y: ZipList[A]) = {
    (x.value, y.value) match {
      case (Left(a), Left(b)) => Ord[A].compare(a, b)
      case (Left(a), Right(b)) => Ord.compareLists(List(a), b)
      case (Right(a), Left(b)) => Ord.compareLists(List(b), a)
      case (Right(a), Right(b)) => Ord.compareLists(a, b)
    }
  }

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] = (x: ZipList[A], y: ZipList[A]) => {
    x.zipWith(y)(Semigroup[A].combine)
  }

  implicit def zipListMonoid[A: Monoid]: Monoid[ZipList[A]] = new Monoid[ZipList[A]] {
    override def empty: ZipList[A] = Repeat(Monoid[A].empty)

    override def combine(x: ZipList[A], y: ZipList[A]): ZipList[A] = {
      x.zipWith(y)(Monoid[A].combine)
    }
  }

  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] = (x: ZipList[A], y: ZipList[A]) => {
    Eq[Either[A, List[A]]].equal(x.value, y.value)
  }

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] = (x: ZipList[A], y: ZipList[A]) => {
    ordZl(x, y)
  }

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = new Num[ZipList[A]] {
    override def fromInt(x: Int): ZipList[A] = {
      Repeat(Num[A].fromInt(x))
    }

    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = {
      x.zipWith(y)(Num[A].plus)
    }

    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = {
      x.zipWith(y)(Num[A].times)
    }

    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = {
      ordZl(x, y)
    }
  }

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = new Integ[ZipList[A]] {
    override def quotRem(x: ZipList[A], y: ZipList[A]): (ZipList[A], ZipList[A]) = {
      x.zipWith(y)(Integ[A].quot) -> x.zipWith(y)(Integ[A].rem)
    }

    override def fromInt(x: Int): ZipList[A] = {
      Repeat(Integ[A].fromInt(x))
    }

    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(Integ[A].plus)

    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = x.zipWith(y)(Integ[A].times)

    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = {
      ordZl(x, y)
    }
  }

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = new Frac[ZipList[A]] {
    override def div(x: ZipList[A], y: ZipList[A]): ZipList[A] = {
      x.zipWith(y)(Frac[A].div)
    }

    override def fromInt(x: Int): ZipList[A] = {
      Repeat(Frac[A].fromInt(x))
    }

    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] = {
      x.zipWith(y)(Frac[A].plus)
    }

    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = {
      x.zipWith(y)(Frac[A].times)
    }

    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = {
      ordZl(x, y)
    }
  }
}
