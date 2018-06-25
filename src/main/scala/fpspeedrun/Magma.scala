package fpspeedrun
import cats.{ Eval, Foldable }

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

  // Right-bound
//  def apply[T](x: T, xs: T*): BinTree[T] = xs match {
//    case Seq() => Leaf(x)
//    case _ =>
//      val reversed = (x +: xs).reverse
//      reversed.tail.foldLeft(Leaf(reversed.head): BinTree[T])((acc, x) => Branch(Leaf(x), acc))
//  }

  // Left-bound
  def apply[T](x: T, xs: T*): BinTree[T] =
    xs.foldLeft(Leaf(x): BinTree[T])((acc, x) => Branch(acc, Leaf(x)))

  implicit val binTreeFoldable: Foldable[BinTree] = new Foldable[BinTree] {
    override def foldLeft[A, B](fa: BinTree[A], b: B)(f: (B, A) => B): B = {
      def loop(current: BinTree[A], acc: B): Eval[B] = current match {
        case Leaf(x) => Eval.now(f(acc, x))
        case Branch(l, r) =>
          for {
            left <- Eval.defer(loop(l, acc))
            both <- Eval.defer(loop(r, left))
          } yield both
      }
      loop(fa, b).value
    }

    override def foldRight[A, B](fa: BinTree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(current: BinTree[A], acc: Eval[B]): Eval[B] = current match {
        case Leaf(x) => f(x, acc)
        case Branch(l, r) => loop(l, Eval.defer(loop(r, acc)))
      }
      loop(fa, lb)
    }
  }
}

trait StdMagmaInstances extends StdSemigroupInstances[Magma]
