package fpspeedrun

import fpspeedrun.syntax.eq._

import scala.annotation.tailrec

trait Eq[T] {
  def ===(x: T, y: T): Boolean
  def =!=(x: T, y: T): Boolean = ! ===(x, y)
}

object Eq {
  implicit def compareList[T: Eq]: Eq[List[T]] =
    (a: List[T], b: List[T]) => a.length == b.length && a.zip(b).forall(x => x._1 === x._2)

  object laws {
    def symmetry[T: Eq](x: T, y: T): Boolean =
      (x === y) == (y === x)

    def transitivity[T: Eq](x: T, y: T, z: T): Boolean =
      !((x === y) && (y === z)) || (x === z)

    def reflectivity[T: Eq](x: T): Boolean =
      x === x
  }

  def fromEquals[T]: Eq[T] = _ == _

  implicit val intEq: Eq[Int] = fromEquals
  implicit val longEq: Eq[Long] = fromEquals

  /** простой вариант */
  implicit def vectorEq[A: Eq]: Eq[Vector[A]] =
    (xs, ys) => xs.view.zip(ys).forall { case (x, y) => x === y }

  /** я у мамы оптимизатор */
  implicit def listEq[A: Eq]: Eq[List[A]] = {
    @tailrec def go(xs: List[A], ys: List[A]): Boolean =
      xs match {
        case Nil => ys.isEmpty
        case x :: xt => ys match {
          case Nil => false
          case y :: yt => x === y && go(xt, yt)
        }
      }

    go
  }
}
