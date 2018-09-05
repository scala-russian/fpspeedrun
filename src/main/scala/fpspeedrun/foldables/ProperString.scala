package fpspeedrun.foldables

import cats.{Eval, Foldable}

final case class ProperString[A](value: String)

object ProperString {
  implicit def foldableProperString: Foldable[ProperString] = new Foldable[ProperString] {
    override def foldLeft[A, B](fa: ProperString[A], b: B)(f: (B, A) => B): B =
      fa.value match {
        case str if str.length > 0 =>
          foldLeft(
            ProperString(str.substring(1)),
            f(b, str.substring(0, 1).asInstanceOf[A])
          )((b: B, a: String) => f(b, a.asInstanceOf[A]))
        case _ => b
      }

    override def foldRight[A, B](fa: ProperString[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.value match {
        case str if str.length > 0 =>
          foldRight(
            ProperString(str.substring(0, str.length - 1)),
            f(str.substring(str.length - 1).asInstanceOf[A], lb)
          )((a: String, b: Eval[B]) => f(a.asInstanceOf[A], b))
        case _ => lb
      }
  }
}
