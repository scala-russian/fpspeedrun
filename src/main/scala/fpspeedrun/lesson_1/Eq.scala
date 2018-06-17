package fpspeedrun.lesson_1
import fpspeedrun.syntax.eq._

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {
  object Laws {
    def symmetry[T : Eq](x: T, y: T): Boolean = (x ==== y) == (y ==== x)
    def transitivy[T : Eq](x: T, y: T, z: T): Boolean = !(x ==== y && y ==== z) || (x ==== z)
    def reflexive[T : Eq](x: T): Boolean = x == x
  }

  implicit def compareList[T: Eq]: Eq[Seq[T]] =
    (x: Seq[T], y: Seq[T]) => x.lengthCompare(y.size) == 0 && (x zip y).forall {
      case (l, r) => l ==== r
    }
}
