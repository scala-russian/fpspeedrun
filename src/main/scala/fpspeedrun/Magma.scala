package fpspeedrun
import simulacrum.typeclass

import scala.annotation.tailrec

@typeclass
trait Magma[T] {
  def merge(x: T, y: T): T
}

object Magma extends StdMagmaInstances {
  implicit val freeMagma: FreeConstruct[Magma, BinTree] =
    new FreeConstruct[Magma, BinTree] {
      override def embed[T](x: T): BinTree[T]     = Leaf(x)
      override def instance[T]: Magma[BinTree[T]] = Branch(_, _)
      override def mapInterpret[A, B](fa: BinTree[A])(f: A => B)(implicit instance: Magma[B]): B = {
        @tailrec def collector(queue: List[BinTree[A]], acc: B): B = queue match {
          case Nil => acc
          case Branch(l, r) :: qs => collector(l :: r :: qs, acc)
          case Leaf(l) :: qs => collector(qs, instance.merge(acc, f(l)))
        }

        @tailrec def reducer(queue: List[BinTree[A]]): B = queue match {
          case Branch(l, r) :: qs => reducer(l :: r :: qs)
          case Leaf(l) :: qs => collector(qs, f(l))
        }

        reducer(fa :: Nil)
      }
    }
}

sealed trait BinTree[T] {
  private def nodesCount: Int =
    implicitly[FreeConstruct[Magma, BinTree]].mapInterpret(this)(_ => 1)(_ + _)

  override def toString: String = this match {
    case Leaf(x) => s"Leaf($x)"
    case Branch(l, r) => s"Branch(${l.nodesCount} node(s), ${r.nodesCount} node(s))"
  }
}

final case class Leaf[T](x: T)                           extends BinTree[T]
final case class Branch[T](x: BinTree[T], y: BinTree[T]) extends BinTree[T]

object BinTree {
  def apply[T](x: T, xs: T*): BinTree[T] =
    xs.foldLeft(Leaf(x): BinTree[T])((acc, x) => Branch(acc, Leaf(x)))
}

trait StdMagmaInstances extends StdSemigroupInstances[Magma]
