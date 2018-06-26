package fpspeedrun

trait Magma[T] {
  def combine(x: T, y: T): T
}

sealed trait FreeMagma[T]
final case class Leaf[T](x: T) extends FreeMagma[T]
final case class Branch[T](x: FreeMagma[T], y: FreeMagma[T]) extends FreeMagma[T]

object FreeMagma {
  implicit def freeMagmaMagma[T]: Magma[FreeMagma[T]] =
    (x: FreeMagma[T], y: FreeMagma[T]) => Branch(x, y)

  def apply[T](x: T, y: T*): FreeMagma[T] = {
    def construct(args: Seq[T]): FreeMagma[T] = {
      args match {
        case Seq(v) => Leaf(v)
        case _ =>
          val (l, r) = args.splitAt(args.size / 2)
          Branch(construct(l), construct(r))
      }
    }
    construct(x +: y)
  }
}