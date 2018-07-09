package fpspeedrun.foldables
import cats.Foldable

final case class Iter[F[_]: Foldable, A](x: F[A]) extends Iterable[A]{
  override def iterator: Iterator[A] = ???
}

final case class Fold[A](xs: Iterable[A])

object Fold{
  implicit val foldable: Foldable[Fold] = ???
}


