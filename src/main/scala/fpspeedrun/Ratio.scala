package fpspeedrun

import syntax.num._
import syntax.integ._
import syntax.eq._
import syntax.ord._

final case class Ratio[T] private (num: T, den: T) {
  override def toString = s"$num \\ $den"
}

object Ratio {
  def make[T: Integ](x: T, y: T): Ratio[T] =
    if (y === zero[T]) Ratio(zero[T], zero[T])
    else {
      val gcd = x gcd y
      Ratio(x / gcd, y / gcd)
    }

  implicit def ratioFrac[T](implicit T: Integ[T]): Frac[Ratio[T]] = new Frac[Ratio[T]] {
    override def div(x: Ratio[T], y: Ratio[T]): Ratio[T] =
      make(x.num * y.den, x.den * y.num)
    override def fromInt(x: Int): Ratio[T] =
      make(x.toNum[T], T.one)
    override def plus(x: Ratio[T], y: Ratio[T]): Ratio[T] =
      make(x.num * y.den + x.den * y.num, x.den * y.den)
    override def times(x: Ratio[T], y: Ratio[T]): Ratio[T] =
      make(x.num * y.num, x.den * y.den)
    override def compare(x: Ratio[T], y: Ratio[T]): Ord.Compare =
      x.num * y.den compare x.den * y.num
  }
}
