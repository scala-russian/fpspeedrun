package fpspeedrun.foldables
import cats.Foldable
import cats.Eval
import cats.instances.stream._
import cats.syntax.foldable._

final case class Iter[F[_]: Foldable, A](x: F[A]) extends Iterable[A]{
  override def iterator: Iterator[A] = {
    Foldable[F].foldRight(x, Eval.now(Iterator[A]())) { (v, acc) =>
      Eval.now {
        val as = Iterator(v)
        as ++ acc.value
      }
    }.value
  }
}


final case class Fold[A](xs: Iterable[A])

object Fold {
  implicit val foldable: Foldable[Fold] = new Foldable[Fold] {
    override def foldLeft[A, B](fa: Fold[A], b: B)(f: (B, A) => B): B = {
      fa.xs.foldLeft(b)(f)
    }

    override def foldRight[A, B](fa: Fold[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      fa.xs.toStream.foldr(lb)(f)
    }
  }
}
