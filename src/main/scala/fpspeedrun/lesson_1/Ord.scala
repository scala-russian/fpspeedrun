package fpspeedrun.lesson_1

import fpspeedrun.lesson_1.Ord.Compare
import fpspeedrun.lesson_1.Ord.Compare.{EQ, GT, LT}
import fpspeedrun.syntax.ord._

trait Ord[T] extends Eq[T] {
  def <>(x: T, y: T): Compare

  override def ===(x: T, y: T): Boolean = <>(x, y) == EQ
}

object Ord {
  sealed trait Compare extends Product with Serializable
  object Compare {
    case object LT extends Compare
    case object EQ extends Compare
    case object GT extends Compare
  }

  implicit def compareSeq[T : Ord]: Ord[Seq[T]] =
    (x: Seq[T], y: Seq[T]) =>
      x.lengthCompare(y.size) match {
        case v if v > 0 => GT
        case v if v < 0 => LT
        case _ => (x zip y).collectFirst {
          case (l, r) if (l <> r) != EQ  => l <> r
        }.getOrElse(EQ)
      }
}