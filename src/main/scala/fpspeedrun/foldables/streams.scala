package fpspeedrun.foldables
import cats.{Eval, Foldable}
import cats.syntax.foldable._

final case class Filter[F[_], A](fa: F[A], p: A => Boolean)

object Filter {
  implicit def foldable[F[_]: Foldable]: Foldable[Filter[F, ?]] = new Foldable[Filter[F, ?]] {
    override def foldLeft[A, B](fa: Filter[F, A], b: B)(f: (B, A) => B): B =
      fa.fa.foldLeft(b)((acc, a) => if (fa.p(a)) f(acc, a) else acc)

    override def foldRight[A, B](fa: Filter[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.fa.foldRight(lb)((a, acc) => if (fa.p(a)) f(a, acc) else acc)
  }
}

final case class Map[F[_], A, B](fa: F[A], p: A => B)

object Map {
  implicit def foldable[F[_]: Foldable, T]: Foldable[Map[F, T, ?]] = new Foldable[Map[F, T, ?]] {
    override def foldLeft[A, B](fa: Map[F, T, A], b: B)(f: (B, A) => B): B =
      fa.fa.foldLeft(b)((acc, a) => f(acc, fa.p(a)))

    override def foldRight[A, B](fa: Map[F, T, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.fa.foldRight(lb)((a, acc) => f(fa.p(a), acc))
  }
}

final case class Collect[F[_], A, B](fa: F[A], pf: PartialFunction[A, B])

object Collect {
  implicit def foldable[F[_]: Foldable, T]: Foldable[Collect[F, T, ?]] = new Foldable[Collect[F, T, ?]] {
    override def foldLeft[A, B](fa: Collect[F, T, A], b: B)(f: (B, A) => B): B =
      fa.fa.foldLeft(b)((acc, a) => if (fa.pf.isDefinedAt(a)) f(acc, fa.pf(a)) else acc)

    override def foldRight[A, B](fa: Collect[F, T, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.fa.foldRight(lb)((a, acc) => if (fa.pf.isDefinedAt(a)) f(fa.pf(a), acc) else acc)
  }
}

final case class FlatMap[F[_], A, G[_], B](fa: F[A], f: A => G[B])

object FlatMap {
  implicit def foldable[F[_]: Foldable, T, G[_]: Foldable]: Foldable[FlatMap[F, T, G, ?]] = new Foldable[FlatMap[F, T, G, ?]] {
    override def foldLeft[A, B](fa: FlatMap[F, T, G, A], b: B)(f: (B, A) => B): B =
      fa.fa.foldLeft(b)((acc, a) => fa.f(a).foldLeft(acc)(f))

    override def foldRight[A, B](fa: FlatMap[F, T, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.fa.foldRight(lb)((a, acc) => fa.f(a).foldRight(acc)(f))
  }
}