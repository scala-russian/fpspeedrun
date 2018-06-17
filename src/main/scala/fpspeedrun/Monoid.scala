package fpspeedrun

import syntax.semigroup._

trait Monoid[T] extends Semigroup[T] {
  def empty: T
}

object Monoid {
  object laws {
    def associativity[T](x: T, y: T, z: T)(implicit m: Monoid[T]): Boolean = ((x |+| y) |+| z) == (x |+| (y |+| z))
    def leftIdentity[T](x: T)(implicit m: Monoid[T]): Boolean              = (m.empty |+| x) == x
    def rightIndentity[T](x: T)(implicit m: Monoid[T]): Boolean            = (x |+| m.empty) == x
  }

  def combineListVia[U[_]] = new CombineListVia[U]

  class CombineListVia[U[_]] {
    def apply[T](list: List[T])(implicit iso: Iso[T, U[T]], m: Monoid[U[T]]): T =
      iso.unwrap(list.foldRight(m.empty)((el: T, acc: U[T]) => iso.wrap(el) |+| acc))
  }
}
