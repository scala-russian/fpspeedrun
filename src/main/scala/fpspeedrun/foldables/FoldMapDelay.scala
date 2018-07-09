package fpspeedrun
package foldables

import cats.{Eval, Foldable, Monoid}
import newts.Dual
import syntax.delay._
import cats.syntax.semigroup._

abstract class FoldMapDelay[F[_]] extends Foldable[F] {
  def foldMapDelay[A, B: Monoid: Delay](fa: F[A])(f: A => B): B

  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = ???
  override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
}

object FoldMapDelay {

  implicit val streamInstance: Foldable[Stream] = ???
}
