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
  implicit def foldable[F[_]: Foldable, X]: Foldable[Collect[F, X, ?]] = new Foldable[Collect[F, X, ?]] {
    override def foldLeft[A, B](fa: Collect[F, X, A], b: B)(f: (B, A) => B): B = {
      fa.fa.foldLeft(b) { (acc, el) => fa.pf.lift(el).fold(acc)(f(acc, _)) }
    }

    override def foldRight[A, B](fa: Collect[F, X, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      fa.fa.foldRight(lb){ (el, acc) => fa.pf.lift(el).fold(acc)(f(_, acc)) }
    }
  }
}

final case class FlatMap[F[_], X, G[_], A](fa: F[X], f: X => G[A])

object FlatMap {
  implicit def foldable[F[_]: Foldable, X, G[_]: Foldable]: Foldable[FlatMap[F, X, G, ?]] = new Foldable[FlatMap[F, X, G, ?]] {
    override def foldLeft[A, B](fa: FlatMap[F, X, G, A], b: B)(f: (B, A) => B): B = {
      fa.fa.foldLeft(b)((acc, x) => fa.f(x).foldLeft(acc)(f))
    }

    override def foldRight[A, B](fa: FlatMap[F, X, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      fa.fa.foldRight(lb)((x, acc) => fa.f(x).foldRight(acc)(f))
    }
  }
}

object Main extends App {
  import Collect._
  import FlatMap._

  import cats.instances.stream._
  import cats.instances.list._

  val pf: PartialFunction[Int, Int] = { case x: Int if x > 3 => x }
  val collect: Collect[List, Int, Int] = Collect(List(1,2,3,4,5), pf)
  require(Iter(collect).iterator.toStream.foldl(0)(_ + _) == 9)
  require(Iter(collect).iterator.toStream.foldr(Eval.now(0))((x, acc) => acc.map(_ + x)).value == 9)

  val f = (x: Int) => List[Int](x, x)
  val flatMap = FlatMap[List, Int, List, Int](List(1,2,3,4,5), f)
  require(Iter(flatMap).iterator.toStream.foldl(0)(_ + _) == 30)
  require(Iter(flatMap).iterator.toStream.foldr(Eval.now(0))((x, acc) => acc.map(_ + x)).value == 30)
}
