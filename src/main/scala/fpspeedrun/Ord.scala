package fpspeedrun

import fpspeedrun.Ord.Compare
import fpspeedrun.syntax.ord._

trait Ord[T] extends Eq[T]{
  def compare(x: T, y: T): Compare

  override def ===(x: T, y: T): Boolean = compare(x, y) == Compare.EQ
}

object Ord{
  sealed trait Compare
  object Compare{
    case object LT extends Compare //less than
    case object EQ extends Compare //equals to
    case object GT extends Compare //greater than
  }

  implicit def orderList[T](implicit ord: Ord[T]): Ord[Seq[T]] = (x, y) => x lengthCompare y.length match {
    case cmp if cmp < 0 => Compare.LT
    case cmp if cmp > 0 => Compare.GT
    case _ => x.zip(y)
      .collectFirst { case (l, r) if (l <> r) != Compare.EQ => l <> r }
      .getOrElse(Compare.EQ)
  }

  implicit val compareLong: Ord[Long] = (x,y) => x compare y match {
    case cmp if cmp < 0 => Compare.LT
    case 0 => Compare.EQ
    case _ => Compare.GT
  }

}

