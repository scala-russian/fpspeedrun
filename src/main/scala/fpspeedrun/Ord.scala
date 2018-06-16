package fpspeedrun
import fpspeedrun.Ord._
import fpspeedrun.syntax.ord._

trait Ord[T] extends Eq[T]{
  def compare(x: T, y: T): Compare

  def ===(x: T, y: T): Boolean = {
    compare(x, y) match  {
      case Compare.EQ => true
      case _ => false
    }
  }

  def >(x: T, y: T): Boolean = {
    compare(x, y) match {
      case Compare.GT => true
      case _ => false
    }
  }

  def <(x: T, y: T): Boolean = {
    compare(x, y) match {
      case Compare.LT => true
      case _ => false
    }
  }

  def <>(x: T, y: T): Compare = {
    compare(x, y)
  }
}

object Ord{
  sealed trait Compare
  object Compare{
    case object LT extends Compare
    case object EQ extends Compare
    case object GT extends Compare
  }

  implicit def listOrdCompare[T: Ord](implicit ord: Ord[T]): Ord[List[T]] = (first: List[T], second: List[T]) => {
    first.zip(second).collectFirst {
      case (x, y) if ord.<>(x, y) != Compare.EQ => ord.<>(x,y)
    }.getOrElse {
      (first.size, second.size) match {
        case (x, y) if x > y => Compare.GT
        case (x, y) if x < y => Compare.LT
        case (x, y) if x == y => Compare.EQ
      }
    }
  }
}
