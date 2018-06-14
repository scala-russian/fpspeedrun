package fpspeedrun

trait Eq[T] {
  def ===(x: T, y: T): Boolean = ! !==(x, y)
  def !==(x: T, y: T): Boolean = ! ===(x, y)
}

object Eq {

  implicit class EqOps[T](val x: T) extends AnyVal{
    def ===(y: T)(implicit eq: Eq[T]): Boolean = eq.===(x, y)
    def !==(y: T)(implicit eq: Eq[T]): Boolean = eq.!==(x, y)
  }

  implicit val ratioEq: Eq[Ratio] = new Eq[Ratio] {
    override def ===(x: Ratio, y: Ratio): Boolean = x.numerator * y.denominator == y.numerator * x.denominator
  }

  implicit def listEq[T : Eq]: Eq[List[T]] = new Eq[List[T]] {
    override def ===(x: List[T], y: List[T]): Boolean = x.size == y.size && (x zip y forall { case (a: T, b: T) => implicitly[Eq[T]].===(a, b) })
  }

}