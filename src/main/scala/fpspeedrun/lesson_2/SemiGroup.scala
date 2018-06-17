package fpspeedrun.lesson_2

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
trait SemiGroup[T] {
  def combine(x: T, y: T): T
}

object SemiGroup {
  import fpspeedrun.syntax.semigroup._

  object Laws {
    def associativity[T : SemiGroup](x: T, y: T, z: T): Boolean = {
      ((x |+| y) |+| z) == (x |+| (y |+| z))
    }
  }

  implicit val stringSemi: SemiGroup[String] = (x: String, y: String) => x + y

  def combineSeq[T : SemiGroup](seq: Seq[T]): Option[T] = {
    seq.reduceOption((l, r) => l |+| r)
  }

  def combineViaSeq[W[_]] = new CombineViaSeq[W]

  class CombineViaSeq[W[_]] {
    def apply[U](seq:Seq[U])(implicit iso: Iso[U, W[U]], semiGroup: SemiGroup[W[U]]): Option[U] = {
      seq.reduceOption((l, r) => iso.unwrap(iso.wrap(l) |+| iso.wrap(r)))
    }
  }
}

final case class Sum[T](v: T) extends AnyVal
final case class Prod[T](v: T) extends AnyVal


object Sum {
  implicit def sumIso[T]: Iso[T, Sum[T]] = new Iso[T, Sum[T]] {
    override def wrap(x: T): Sum[T] = Sum(x)

    override def unwrap(x: Sum[T]): T = x.v
  }
}

object Prod {
  implicit def prodIso[T]: Iso[T, Prod[T]] = new Iso[T, Prod[T]] {
    override def wrap(x: T): Prod[T] = Prod(x)

    override def unwrap(x: Prod[T]): T = x.v
  }
}
