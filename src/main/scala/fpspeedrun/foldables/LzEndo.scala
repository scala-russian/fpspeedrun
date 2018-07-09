package fpspeedrun.foldables

import cats.{Eval, Monoid}

final case class LzEndo[A](run: Eval[A] => Eval[A]) extends AnyVal

object LzEndo {
  implicit def endoInstance[A]: Monoid[LzEndo[A]] with Delay[LzEndo[A]] = ???
}
