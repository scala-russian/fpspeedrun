package fpspeedrun
import fpspeedrun.Ord.Compare
import fpspeedrun.syntax.ord._

trait Ord[T] extends Eq[T] {
  def compare(x: T, y: T): Compare

  def ===(x: T, y: T): Boolean = {
    compare(x, y) match {
      case Compare.EQ => true
      case _          => false
    }
  }
}

object Ord {
  sealed trait Compare
  object Compare {
    case object LT extends Compare //less than
    case object EQ extends Compare //equals to
    case object GT extends Compare //greater than
  }

  implicit def ordSeq[T: Ord]: Ord[Seq[T]] = (x: Seq[T], y: Seq[T]) => {
    x.zip(y)
      .collectFirst { case (a, b) if (a <> b) != Compare.EQ => a <> b }
      .getOrElse {
        (x.size, y.size) match {
          case (a, b) if a > b  => Compare.GT
          case (a, b) if a < b  => Compare.LT
          case (a, b) if a == b => Compare.EQ
        }
      }
  }
}
