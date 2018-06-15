package fpspeedrun

trait Eq[T] {
  def ===(x: T, y: T): Boolean = ! !==(x, y)
  def !==(x: T, y: T): Boolean = ! ===(x, y)
}

object Eq {

  import syntax.eq.EqOps

  implicit def listEq[T : Eq]: Eq[List[T]] = new Eq[List[T]] {
    override def ===(x: List[T], y: List[T]): Boolean =
      x.size == y.size && ( x zip y forall { case (a: T, b: T) => a === b } )
  }

}