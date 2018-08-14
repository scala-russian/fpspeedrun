package fpspeedrun.foldables

import cats.{Eval, Monoid}

final case class LzEndo[A](run: Eval[A] => Eval[A]) extends AnyVal

object LzEndo {
  implicit def endoInstance[A]: Monoid[LzEndo[A]] with Delay[LzEndo[A]] =
    new Monoid[LzEndo[A]] with Delay[LzEndo[A]] {
      override def empty: LzEndo[A] = LzEndo(identity)

      override def delay(x: => LzEndo[A]): LzEndo[A] = LzEndo(y => Eval.defer(x.run(y)))

      override def combine(x: LzEndo[A], y: LzEndo[A]): LzEndo[A] = {
        LzEndo(ev => Eval.later(()).flatMap(_ => Eval.defer(y.run(Eval.defer(x.run(ev))))))
      }
    }
}
