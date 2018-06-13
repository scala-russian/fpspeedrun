package fpspeedrun
import fpspeedrun.Ord.Compare
import fpspeedrun.syntax.ord._

trait Ord[T] extends Eq[T] {
  def compare(x: T, y: T): Compare
}

object Ord {
  sealed trait Compare
  object Compare {
    case object LT extends Compare //less than
    case object EQ extends Compare //equals to
    case object GT extends Compare //greater than
    case object UNDEFINED extends Compare
  }

  implicit def ordList[T](implicit ord: Ord[T]): Ord[Seq[T]] = new Ord[Seq[T]] {

    override def compare(x: Seq[T], y: Seq[T]): Compare = {
      if (x.size != y.size) {
        return Compare.UNDEFINED
      }
      x.zip(y)
        .map {
          case (l, r) => l <> r
        }
        .find(c => c != Compare.EQ)
        .getOrElse(Compare.EQ)
    }

    override def ===(x: Seq[T], y: Seq[T]): Boolean =
      compare(x, y) == Compare.EQ

  }
}
