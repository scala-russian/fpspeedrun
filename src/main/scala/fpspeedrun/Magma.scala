package fpspeedrun
import simulacrum.typeclass

@typeclass
trait Magma[T] {
  def merge(x: T, y: T): T
}

object Magma extends StdMagmaInstances {
  implicit val freeMagma: FreeConstruct[Magma, BinTree] =
    new FreeConstruct[Magma, BinTree] {
      override def embed[T](x: T): BinTree[T]     = Leaf(x)
      override def instance[T]: Magma[BinTree[T]] = Branch(_, _)
      override def mapInterpret[A, B](fa: BinTree[A])(f: A => B)(implicit instance: Magma[B]): B =
        fa match {
          case Leaf(a)          => f(a)
          case Branch(fmx, fmy) => instance.merge(mapInterpret(fmx)(f), mapInterpret(fmy)(f))
        }
    }
}

sealed trait BinTree[T]

final case class Leaf[T](x: T)                           extends BinTree[T]
final case class Branch[T](x: BinTree[T], y: BinTree[T]) extends BinTree[T]

trait StdMagmaInstances extends StdSemigroupInstances[Magma]
