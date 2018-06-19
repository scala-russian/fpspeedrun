package fpspeedrun

import scala.language.higherKinds
import syntax.semigroup._

trait Monoid[T] extends SemiGroup[T] {
  def empty: T
}

object Monoid {
  def apply[T](implicit i: Monoid[T]) = i

  object Laws {
    def identity[T : Monoid](x: T): Boolean = {
      (Monoid[T].empty |+| x) == x &&
        (x |+| Monoid[T].empty) == x
    }

    def associativity[T : Monoid](x: T, y: T, z: T): Boolean = {
      ((x |+| y) |+| z) == (x |+| (y |+| z))
    }
  }

  /**
    * Функция, похожая на SemiGroup#combineList, но для Monoid.
    * В отличие от предыдущей, возвращает не Option[T], а сам T.
    * Для пустого списка, возвращает "единицу" в понятиях моноида.
    */
  def foldList[T : Monoid](list: List[T]): T = {
    list.foldLeft(Monoid[T].empty) {
      case (sum, next) => sum |+| next
    }
  }

  /**
    * #foldList, но с крутым синтаксисом и возможностью через параметр типа передавать
    * желаемое поведение.
    * Паттерн для синтаксиса называется: partial type application
    */
  def foldListVia[U[_]] = new FoldListVia[U]
  class FoldListVia[U[_]] {
    def apply[T](list: List[T])(implicit iso: Iso[T, U[T]], monoid: Monoid[U[T]]): T = {
      val r = list.foldLeft(monoid.empty) {
        (acc, next) => acc |+| iso.wrap(next)
      }
      iso.unwrap(r)
    }
  }

  // monoid instances
  implicit val defaultMonoidInt: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0
    override def combine(x: Int, y: Int): Int = x+y
  }
  implicit val sumMonoidInt: Monoid[Sum[Int]] = new Monoid[Sum[Int]] {
    override def empty: Sum[Int] = Sum(0)
    override def combine(x: Sum[Int], y: Sum[Int]): Sum[Int] = Sum(x.x + y.x)
  }
  implicit val prodMonoidInt: Monoid[Prod[Int]] = new Monoid[Prod[Int]] {
    override def empty: Prod[Int] = Prod(1)
    override def combine(x: Prod[Int], y: Prod[Int]): Prod[Int] = Prod(x.x * y.x)
  }
}