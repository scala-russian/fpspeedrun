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
        @tailrec def impl(toVisit: List[BinTree[A]], acc: List[B]): List[B] =
          if (toVisit.isEmpty) acc
          else {
            toVisit.head match {
              case Leaf(v) => impl(toVisit.tail, f(v) :: acc)
              case Branch(l, r) => impl(r :: l :: toVisit.tail, acc)
            }
          }
        impl(fa :: Nil, Nil).reduce(instance.merge)
      }
    }
}

sealed trait BinTree[T]

final case class Leaf[T](x: T) extends BinTree[T] {
  override def toString: String = x.toString
}

final case class Branch[T](x: BinTree[T], y: BinTree[T]) extends BinTree[T] {
  override def toString: String = s"left -> ${x.toString} right -> ${y.toString}"
}

object BinTree {
  def apply[T](x: T, y: T*): BinTree[T] = {
    @tailrec def construct(args: Seq[BinTree[T]]): BinTree[T] =
      args.sliding(2, 2).map {
        case Seq(l, r) => Branch(l, r)
        case Seq(o) => o
      }.toSeq match {
        case Seq(m) => m
        case xs => construct(xs)
      }
    construct((x +: y).map(Leaf(_)))
  }
}

trait StdMagmaInstances extends StdSemigroupInstances[Magma]
