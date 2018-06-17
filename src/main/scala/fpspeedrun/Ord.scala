package fpspeedrun
import fpspeedrun.Ord.Compare
import fpspeedrun.Ord.Compare.{EQ, GT, LT}

trait Ord[T] extends Eq[T] {
  def compare(x: T, y: T): Compare
}

object Ord {
  sealed trait Compare
  object Compare {
    case object LT extends Compare //less than
    case object EQ extends Compare //equals to
    case object GT extends Compare //greater than
  }

  implicit var intOrd: Ord[Int] = new Ord[Int] {
    override def compare(x: Int, y: Int): Compare = (x, y) match {
      case _ if x > y => GT
      case _ if x < y => LT
      case _          => EQ
    }

    override def ===(x: Int, y: Int): Boolean = x == y
  }
}
