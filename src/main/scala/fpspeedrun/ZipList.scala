package fpspeedrun

final case class ZipList[A](list: List[A]) extends AnyVal

object ZipList {
  implicit def iso[A]: Iso[List[A], ZipList[A]] = new Iso[List[A], ZipList[A]] {
    override def wrap(a: List[A]): ZipList[A] = ZipList(a)
    override def unwrap(b: ZipList[A]): List[A] = b.list
  }

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] = ???

  implicit def zipListMonoid[A: Monoid]: Monoid[ZipList[A]] = ???

  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] = ???

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] = ???

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = ???

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = ???

  implicit def zipListFrac[A: Integ]: Frac[ZipList[A]] = ???
}


