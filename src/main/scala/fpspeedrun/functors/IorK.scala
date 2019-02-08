package fpspeedrun.functors
import cats.Functor

sealed trait IorK[F[_], G[_], A]

object IorK {
  final case class Left[F[_], G[_], A](fa: F[A]) extends IorK[F, G, A]
  final case class Right[F[_], G[_], A](ga: G[A]) extends IorK[F, G, A]
  final case class Both[F[_], G[_], A](fa: F[A], ga: G[A]) extends IorK[F, G, A]

  class IorKFunctor[F[_] : Functor, G[_] : Functor] extends Functor[IorK[F, G, ?]] {
    override def map[A, B](fa: IorK[F, G, A])(f: A => B): IorK[F, G, B] = ???
  }
}
