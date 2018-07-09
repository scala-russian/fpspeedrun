package fpspeedrun.foldables

import cats.{Eval, Monoid}

final case class LzEndo[A](run: Eval[A] => Eval[A]) extends AnyVal

object LzEndo {
  implicit def endoInstance[A]: Monoid[LzEndo[A]] with Delay[LzEndo[A]] =
    new Monoid[LzEndo[A]] with Delay[LzEndo[A]] {
      override def empty: LzEndo[A] = LzEndo(identity)
      override def combine(x: LzEndo[A], y: LzEndo[A]): LzEndo[A] =
        LzEndo(a => Eval.defer(x.run(Eval.later(y).flatMap(_.run(a)))))
      override def delay(x: => LzEndo[A]): LzEndo[A] = LzEndo(a => Eval.later(x).flatMap(_.run(a)))
    }
}
