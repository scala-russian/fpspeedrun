package fpspeedrun

import syntax.semigroup._

trait Semigroup[T] {
  def combine(x: T, y: T): T
}

object Semigroup {
  object laws {
    def associativity[T](x: T, y: T, z: T)(implicit semi: Semigroup[T]): Boolean =
      ((x |+| y) |+| z) == (x |+| (y |+| z))
  }

  implicit val stringSemi: Semigroup[String] = (x: String, y: String) => x + y

  def combineList[T: Semigroup](list: List[T]): Option[T] = list.reduceOption(_ |+| _)

  def combineListVia[U[_]] = new CombineListVia[U]

  class CombineListVia[U[_]] {
    def apply[T](list: List[T])(implicit iso: Iso[T, U[T]], semi: Semigroup[U[T]]): Option[T] =
      list.reduceOption((a: T, b: T) => iso.unwrap(iso.wrap(a) |+| iso.wrap(b)))
  }
}

final case class Sum[T](x: T)  extends AnyVal
final case class Prod[T](x: T) extends AnyVal

object Sum {
  implicit def sumIso[T]: Iso[T, Sum[T]] = new Iso[T, Sum[T]] {
    override def wrap(x: T): Sum[T]   = Sum(x)
    override def unwrap(x: Sum[T]): T = x.x
  }

  implicit val intSemiSum: Semigroup[Sum[Int]] = (x: Sum[Int], y: Sum[Int]) => Sum(x.x + y.x)
}

object Prod {
  implicit def prodIso[T]: Iso[T, Prod[T]] = new Iso[T, Prod[T]] {
    override def wrap(x: T): Prod[T]   = Prod(x)
    override def unwrap(x: Prod[T]): T = x.x
  }

  implicit val intSemiProd: Semigroup[Prod[Int]] = (x: Prod[Int], y: Prod[Int]) => Prod(x.x * y.x)
}
