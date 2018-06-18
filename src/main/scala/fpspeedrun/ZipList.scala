package fpspeedrun
import syntax.eq._
import syntax.ord._

final case class ZipList[A](list: List[A]) extends AnyVal

object ZipList {
  implicit def iso[A]: Iso[List[A], ZipList[A]] = new Iso[List[A], ZipList[A]] {
    override def wrap(a: List[A]): ZipList[A] = ZipList(a)
    override def unwrap(b: ZipList[A]): List[A] = b.list
  }

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] = ???

  implicit def zipListMonoid[A: Monoid]: Monoid[ZipList[A]] = ???

  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] = _.list === _.list

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] = _.list compare _.list

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = ???

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = ???

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = ???

}


