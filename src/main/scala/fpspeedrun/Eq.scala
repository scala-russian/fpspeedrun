package fpspeedrun
import fpspeedrun.Eq.ops
import simulacrum.{op, typeclass}

import scala.annotation.tailrec

@typeclass
trait Eq[T] {
  @op("===", alias = true)
  def equal(x: T, y: T): Boolean

  @op("=/=", alias = true)
  def notEqual(x: T, y: T): Boolean = !equal(x, y)
}

object Eq extends StdEqInstances {
  import ops._

  def fromEquals[A]: Eq[A] = _ == _

  /** простой вариант */
  implicit def vectorEq[A: Eq]: Eq[Vector[A]] =
    (xs, ys) => xs.view.zip(ys).forall { case (x, y) => x === y }

  /** я у мамы оптимизатор */
  implicit def listEq[A: Eq]: Eq[List[A]] = {
    @tailrec def go(xs: List[A], ys: List[A]): Boolean =
      xs match {
        case Nil => ys.isEmpty
        case x :: xt =>
          ys match {
            case Nil => false
            case y :: yt => x === y && go(xt, yt)
          }
      }
    go
  }
}

trait StdEqInstances extends StdOrdInstances[Eq]{
  import ops._

  implicit def eitherEq[A: Eq, B: Eq]: Eq[Either[A, B]] = (first, second) => (first, second) match {
    case (Left(l1), Left(l2)) => l1 === l2
    case (Right(r1), Right(r2)) => r1 === r2
    case _ => false
  }
}
