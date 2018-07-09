package fpspeedrun.foldables

import cats.Eval
import newts.Dual
import simulacrum.typeclass

@typeclass
trait Delay[A] {
  def delay(x: => A): A
}

sealed trait EvalDelayInstance{
  implicit def evalDelay[A]: Delay[Eval[A]] = x => Eval.defer(x)
}
object Delay extends EvalDelayInstance {
  implicit def dual[A](implicit delay: Delay[A]): Delay[Dual[A]] = x => Dual(delay.delay(x.getDual))
}


