package fpspeedrun
import fpspeedrun.Ord.{Compare, byOrdering}
import fpspeedrun.Ord.Compare.{EQ, GT, LT}
import simulacrum.{op, typeclass}
import syntax.eq._

import scala.annotation.tailrec
import scala.collection.immutable.Seq

@typeclass
trait Ord[T] extends Eq[T] {
  @op("<=>", alias = true) //PHP baby!
  def compare(x: T, y: T): Compare

  override def equal(x: T, y: T): Boolean = compare(x, y) == EQ

  @op(">")
  def gt(x: T, y: T): Boolean = compare(x, y) === GT
  @op("<")
  def lt(x: T, y: T): Boolean = compare(x, y) === LT
  @op(">=")
  def gte(x: T, y: T): Boolean = compare(x, y) =/= LT
  @op("<=")
  def lte(x: T, y: T): Boolean = compare(x, y) =/= GT
}

object Ord extends StdOrdInstances[Ord] {
  import ops._

  sealed trait Compare
  object Compare {
    case object LT extends Compare //less than
    case object EQ extends Compare //equals to
    case object GT extends Compare //greater than

    implicit val eq: Eq[Compare] = Eq.fromEquals
  }

  def byOrdering[T: Ordering]: Ord[T] = new FromOrdering[T]

  @tailrec def compareLists[T: Ord](xs: List[T], ys: List[T]): Compare =
    xs match {
      case Nil => if (ys.isEmpty) EQ else LT
      case x :: xtail =>
        ys match {
          case Nil => GT
          case y :: ytail =>
            val res: Compare = x compare y
            if (res =/= EQ) res else compareLists(xtail, ytail)
        }
    }

  implicit def listOrd[T: Ord]: Ord[List[T]] = compareLists

  implicit def vectorOrd[T: Ord]: Ord[Vector[T]] =
    (xs, ys) =>
      (xs.iterator zip ys.iterator)
        .map((Ord[T].compare _).tupled)
        .find(_ =/= EQ)
        .getOrElse(xs.size <=> ys.size)

  implicit def seqOrd[T: Ord]: Ord[Seq[T]] =
    (xs, ys) =>
      (xs, ys).zipped
        .collectFirst { case Compared(cmp) if cmp =/= EQ => cmp }
        .getOrElse(xs.size <=> ys.size)

  object Compared {
    def unapply[T: Ord](pair: (T, T)): Matched[Compare] =
      Matched(Ord[T].compare(pair._1, pair._2))
  }

  class FromOrdering[T](implicit ord: Ordering[T]) extends Ord[T] {
    override def compare(x: T, y: T): Compare = {
      val res = Ordering[T].compare(x, y)
      if (res == 0) EQ else if (res < 0) LT else GT
    }
  }
}

trait StdOrdInstances[TC[t] >: Ord[t]] extends StdNumInstances[TC] {
  import Ord.ops._

  final implicit val stringOrd: TC[String] = byOrdering

  final implicit val doubleOrd: TC[Double] = byOrdering

  final implicit def optionOrd[A](implicit ev$1: Ord[A]): TC[Option[A]] =
    new Ord[Option[A]] {
      override def compare(x: Option[A], y: Option[A]): Compare = (x, y) match {
        case (None, None)         => EQ
        case (None, Some(_))      => LT
        case (Some(_), None)      => GT
        case (Some(xx), Some(yy)) => xx <=> yy
      }
    }
}
