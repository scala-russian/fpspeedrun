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
  }

  implicit val ordLong: Ord[Long] = new Ord[Long] {
    override def compare(x: Long, y: Long): Compare = x - y match {
      case 0                => Compare.EQ
      case diff if diff > 0 => Compare.GT
      case _                => Compare.LT
    }

    override def ===(x: Long, y: Long): Boolean = compare(x, y) == Compare.EQ
  }

  implicit def ordList[T](implicit ord: Ord[T]): Ord[Seq[T]] = new Ord[Seq[T]] {

    override def compare(x: Seq[T], y: Seq[T]): Compare =
      x.size - y.size match {
        case diff if diff > 0 => Compare.GT
        case diff if diff < 0 => Compare.LT
        case _ =>
          x.zip(y)
            .collectFirst { case (l, r) if (l <> r) != Compare.EQ => l <> r }
            .getOrElse(Compare.EQ)
      }

    override def ===(x: Seq[T], y: Seq[T]): Boolean =
      compare(x, y) == Compare.EQ

  }
}
