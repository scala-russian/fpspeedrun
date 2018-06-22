package fpspeedrun

trait Magma[T] {
  def combine(x: T, y: T): T
}

sealed trait FreeMagma[T]
case class Leaf[T](x: T) extends FreeMagma[T]
case class Branch[T](x: FreeMagma[T], y: FreeMagma[T]) extends FreeMagma[T]

object FreeMagma {

  def apply[T](x: T, y: T*): FreeMagma[T] = y match {
    case Seq()  => Leaf(x)
    case Seq(a) => Branch(Leaf(x), Leaf(a))
    case ys     =>
      val list   = (x +: ys).reverse
      val first +: second +: rest = list

      rest.foldLeft(Branch(Leaf(second), Leaf(first)))((acc, e) => Branch(Leaf(e), acc))
  }
  
  implicit def freeMagmaMagma[T]: Magma[FreeMagma[T]] = (x: FreeMagma[T], y: FreeMagma[T]) => Branch(x, y)

}