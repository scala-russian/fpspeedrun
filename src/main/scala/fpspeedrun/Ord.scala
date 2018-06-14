package fpspeedrun

import fpspeedrun.Ord.{Compare, EQ, GT, LT}

trait Ord[T] extends Eq[T] {
  override def ===(x: T, y: T): Boolean = compare(x, y) == EQ
  def compare(x: T, y: T): Compare
}

object Ord{
  sealed trait Compare

  case object LT extends Compare //less than
  case object EQ extends Compare //equals to
  case object GT extends Compare //greater than

  implicit class OrdOps[T](val x: T) extends AnyVal {
    def ===(y: T)(implicit ord: Ord[T]): Boolean = ord.===(x, y)
    def !==(y: T)(implicit ord: Ord[T]): Boolean = ord.!==(x, y)
    def compare(y: T)(implicit ord: Ord[T]): Compare = ord.compare(x, y)
  }

  implicit val ratioOrd: Ord[Ratio] = (x: Ratio, y: Ratio) => {
    val leftUpToRightDown: Int = x.numerator * y.denominator
    val leftDownToRightUp: Int = x.denominator * y.numerator

    if (leftUpToRightDown > leftDownToRightUp) GT
    else if (leftUpToRightDown < leftDownToRightUp) LT
    else EQ
  }

  implicit def lexicographicalListOrd[T : Ord]: Ord[List[T]] = (x: List[T], y: List[T]) => (x, y) match {
    case (a, b) if a.isEmpty && b.isEmpty  => EQ
    case (a, b) if a.isEmpty && b.nonEmpty => LT
    case (a, b) if a.nonEmpty && b.isEmpty => GT
    case (a :: as, b :: bs)                =>
      val cmp = a compare b
      if (cmp == EQ) as compare bs else cmp
  }
}

object OrdInstances {
  import Ord.OrdOps

  implicit def sizeFirstListOrd[T : Ord]: Ord[List[T]] = (x: List[T], y: List[T]) =>
    if (x.size < y.size) LT
    else if (x.size > y.size) GT
    else x zip y find { case (a, b) => a !== b } match {
      case Some((a, b)) => a compare b
      case None         => EQ
    }

  implicit def sizeOnlyListOrd[T : Ord]: Ord[List[T]] = (x: List[T], y: List[T]) =>
    if (x.size < y.size) LT
    else if (x.size > y.size) GT
    else EQ
}