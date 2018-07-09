package fpspeedrun.foldables
import cats.Foldable

final case class Filter[F[_], A](fa: F[A], p: A => Boolean)

object Filter {
  implicit def foldable[F[_]: Foldable]: Foldable[Filter[F, ?]] = ???
}

final case class Map[F[_], A, B](fa: F[A], p: A => B)

object Map {
  implicit def foldable[F[_]: Foldable, A]: Foldable[Map[F, A, ?]] = ???
}

final case class Collect[F[_], A, B](fa: F[A], pf: PartialFunction[A, B])

object Collect {
  implicit def foldable[F[_]: Foldable, A]: Foldable[Collect[F, A, ?]] = ???
}

final case class FlatMap[F[_], A, G[_], B](fa: F[A], f: A => G[B])

object FlatMap {
  implicit def foldable[F[_]: Foldable, A, G[_]: Foldable]: Foldable[FlatMap[F, A, G, ?]] = ???
}
