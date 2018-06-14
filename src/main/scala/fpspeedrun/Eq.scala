package fpspeedrun

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {
  implicit def eqSeq[T: Eq]: Eq[Seq[T]] = (x: Seq[T], y: Seq[T]) => {
    x.size == y.size && x.zip(y).forall { case (a, b) => implicitly[Eq[T]].===(a, b) }
  }
}
