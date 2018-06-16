package fpspeedrun
import fpspeedrun.syntax.eq._

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {
  implicit def compareList[T: Eq]: Eq[List[T]] = (first: List[T], second: List[T]) => {
    first.size.equals(second.size) && first.zip(second).forall {
      case (x, y) => x === y
    }
  }
}
