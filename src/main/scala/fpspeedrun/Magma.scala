package fpspeedrun

trait Magma[T] {
  def combine(x: T, y: T): T
}

sealed trait FreeMagma[T]

final case class Leaf[T](x: T) extends FreeMagma[T]
final case class Branch[T](l: FreeMagma[T], r: FreeMagma[T]) extends FreeMagma[T]

object FreeMagma {
  implicit def freeMagmaMagma[T]: Magma[FreeMagma[T]] =
    (x: FreeMagma[T], y: FreeMagma[T]) => Branch(x, y)

  def apply[T](x: T, xs: T*): FreeMagma[T] = xs match {
    case Seq() => Leaf(x)
    case _ =>
      val reversed = (x +: xs).reverse
      reversed.tail.foldLeft(Leaf(reversed.head): FreeMagma[T])((acc, x) => Branch(Leaf(x), acc))
  }
}
