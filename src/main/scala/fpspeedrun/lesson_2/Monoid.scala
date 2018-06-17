package fpspeedrun.lesson_2

import fpspeedrun.syntax.semigroup._

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
trait Monoid[T] extends SemiGroup[T] {
  def empty: T
}

object Monoid {

  object Laws {
    def associativity[T: Monoid](x: T, y: T, z: T): Boolean = ((x |+| y) |+| z) == (x |+| (y |+| z))

    def leftIdentity[T](x: T)(implicit monoid: Monoid[T]): Boolean = (monoid.empty |+| x) == x

    def rightIdentity[T](x: T)(implicit monoid: Monoid[T]): Boolean = x == (x |+| monoid.empty)
  }

  def combineViaSeq[W[_]] = new CombineViaSeq[W]

  class CombineViaSeq[W[_]] {
    def apply[U](seq:Seq[U])(implicit iso: Iso[U, W[U]], monoid: Monoid[W[U]]): U = {
      iso.unwrap(seq.foldLeft(monoid.empty) {
        case (acc, v) => acc |+| iso.wrap(v)
      })
    }
  }
}