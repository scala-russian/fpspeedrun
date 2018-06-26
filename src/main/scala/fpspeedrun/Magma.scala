package fpspeedrun

import scala.annotation.tailrec

trait Magma[T] {
  def combine(x: T, y: T): T
}

sealed trait FreeMagma[T]
final case class Leaf[T](x: T) extends FreeMagma[T] {
  override def toString: String = x.toString
}
final case class Branch[T](x: FreeMagma[T], y: FreeMagma[T]) extends FreeMagma[T] {
  override def toString: String = s"left -> ${x.toString} right -> ${y.toString}"
}

object FreeMagma {
  implicit def freeMagmaMagma[T]: Magma[FreeMagma[T]] =
    (x: FreeMagma[T], y: FreeMagma[T]) => Branch(x, y)

  def apply[T](x: T, y: T*): FreeMagma[T] = {

    @tailrec def construct(args: Seq[FreeMagma[T]]): FreeMagma[T] =
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