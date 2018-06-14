package fpspeedrun.lesson_1


trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {
  implicit def compareList[T](implicit eq0: Eq[T]): Eq[Seq[T]] =
    (x: Seq[T], y: Seq[T]) => x.lengthCompare(y.size) == 0 && (x zip y).forall {
      case (l, r) => eq0.===(l, r)
    }
}
