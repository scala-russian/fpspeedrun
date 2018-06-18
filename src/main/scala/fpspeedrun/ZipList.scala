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

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] = ???

  implicit def zipListMonoid[A: Monoid]: Monoid[ZipList[A]] = ???

  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] = ???

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] = ???

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = ???

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = ???

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = ???
}
