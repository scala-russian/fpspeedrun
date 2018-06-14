package fpspeedrun
import fpspeedrun.Ord.Compare

trait Ord[T] extends Eq[T] {
  def compare(x: T, y: T): Compare
}

object Ord {
  sealed trait Compare
  object Compare {
    case object LT //less than
    case object EQ //equals to
    case object GT //greater than
  }
}
