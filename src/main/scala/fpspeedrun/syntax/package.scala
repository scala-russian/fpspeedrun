package fpspeedrun.syntax
import fpspeedrun._
import fpspeedrun.foldables.Delay

object num extends Num.ToNumOps {
  def zero[T: Num]: T = Num[T].zero
  def one[T: Num]: T  = Num[T].one
  implicit class IntNumOps(val x: Int) extends AnyVal {
    def toNum[T](implicit num: Num[T]): T = num.fromInt(x)
  }
}

object integ extends Integ.ToIntegOps

object frac extends Frac.ToFracOps

object ratio {
  implicit class RatioOps[T](val x: T) extends AnyVal {
    def \\(y: T)(implicit int: Integ[T]): Ratio[T] = Ratio.make(x, y)
    def toRatio(implicit int: Integ[T]): Ratio[T]  = Ratio.make(x, int.one)
  }
}

object delay extends Delay.ToDelayOps {
  implicit class DelayOps[A](x: => A) {
    def delay(implicit delay: Delay[A]): A = delay.delay(x)
  }
}
