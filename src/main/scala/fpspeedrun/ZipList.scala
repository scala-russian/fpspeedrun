package fpspeedrun

trait ZipList[A] {
  def value: Either[A, List[A]]
}

object ZipList {
  final case class Repeat[A](single: A) extends ZipList[A] {
    override def value = Left(single)
  }
  final case class Final[A](list: List[A]) extends ZipList[A] {
    override def value = Right(list)
  }

  def apply[A](list: List[A]): ZipList[A] = Final(list)

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] = ???

  implicit def zipListMonoid[A: Monoid]: Monoid[ZipList[A]] = ???

  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] = ???

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] = ???

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = ???

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = ???

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = ???
}
