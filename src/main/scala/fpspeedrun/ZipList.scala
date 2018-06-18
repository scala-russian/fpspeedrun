package fpspeedrun
import fpspeedrun.Ord.Compare.EQ
import syntax.eq._
import syntax.ord._

trait ZipList[A] {
  def value: Either[A, List[A]]
}

object ZipList {
  final case class Repeat[A](single: A) extends ZipList[A] {
    override def value = Left(single)
  }
  final case class Finite[A](list: List[A]) extends ZipList[A] {
    override def value = Right(list)
  }

  def apply[A](list: List[A]): ZipList[A] = Finite(list)
  def repeat[A](value: A): ZipList[A] = Repeat(value)

  implicit def zipListSemigroup[A: Semigroup]: Semigroup[ZipList[A]] = ???

  implicit def zipListMonoid[A: Monoid]: Monoid[ZipList[A]] = ???

  implicit def zipListEq[A: Eq]: Eq[ZipList[A]] = _.value === _.value

  implicit def zipListOrd[A: Ord]: Ord[ZipList[A]] = new ZipListOrd[A]

  implicit def zipListNum[A: Num]: Num[ZipList[A]] = ???

  implicit def zipListInteg[A: Integ]: Integ[ZipList[A]] = ???

  implicit def zipListFrac[A: Frac]: Frac[ZipList[A]] = ???

  class ZipListOrd[A: Ord] extends Ord[ZipList[A]] {
    override def compare(xs: ZipList[A], ys: ZipList[A]): Ord.Compare = (xs, ys) match {
      case (Repeat(x), Repeat(y)) => x compare y
      case (Repeat(x), Finite(y)) => y.view.map(x compare _).find(_ =/= EQ).getOrElse(EQ)
      case (Finite(x), Repeat(y)) => x.view.map(_ compare y).find(_ =/= EQ).getOrElse(EQ)
      case (Finite(x), Finite(y)) => x compare y
    }
  }

  class ZipListNum[A: Num] extends Num[ZipList[A]]{
    override def fromInt(x: Int): ZipList[A] = repeat(x)
    override def plus(x: ZipList[A], y: ZipList[A]): ZipList[A] =
    override def times(x: ZipList[A], y: ZipList[A]): ZipList[A] = ???
    override def compare(x: ZipList[A], y: ZipList[A]): Ord.Compare = ???
  }

}
