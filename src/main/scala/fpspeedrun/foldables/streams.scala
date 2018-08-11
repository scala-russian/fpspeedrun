package fpspeedrun.foldables
import cats.{Eval, Foldable}
import cats.syntax.foldable._

final case class Filter[F[_], A](fa: F[A], p: A => Boolean)

object Filter {
  implicit def foldable[F[_]: Foldable]: Foldable[Filter[F, ?]] = new Foldable[Filter[F, ?]] {
    override def foldLeft[A, B](fa: Filter[F, A], b: B)(f: (B, A) => B): B = {
      fa.fa.foldLeft(b)((acc, el) => if (fa.p(el)) f(acc, el) else acc)
    }
    override def foldRight[A, B](fa: Filter[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      fa.fa.foldRight(lb)((el, acc) => if (fa.p(el)) f(el, acc) else acc)
    }
  }
}

final case class Map[F[_], A, B](fa: F[A], p: A => B)

object Map {
  implicit def foldable[F[_]: Foldable, X]: Foldable[Map[F, X, ?]] = new Foldable[Map[F, X, ?]] {
    override def foldLeft[A, B](fa: Map[F, X, A], b: B)(f: (B, A) => B): B = {
      fa.fa.foldLeft(b) { (acc, el) => f(acc, fa.p(el)) }
    }

    override def foldRight[A, B](fa: Map[F, X, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      fa.fa.foldRight(lb) { (el, acc) => f(fa.p(el), acc) }
    }
  }
}

final case class Collect[F[_], A, B](fa: F[A], pf: PartialFunction[A, B])

object Collect {
  implicit def foldable[F[_]: Foldable, A]: Foldable[Collect[F, A, ?]] = ???
}

final case class FlatMap[F[_], A, G[_], B](fa: F[A], f: A => G[B])

object FlatMap {
  implicit def foldable[F[_]: Foldable, A, G[_]: Foldable]: Foldable[FlatMap[F, A, G, ?]] = ???
}
