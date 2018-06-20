package fpspeedrun
import simulacrum.typeclass

/** Pointed type */
@typeclass
trait Default[A] {
  def default: A
}

object Default {
  implicit val freeConstruct: FreeConstruct[Default, Option] =
    new FreeConstruct[Default, Option] {
      override def embed[T](x: T): Option[T] = Some(x)
      override def instance[T]: Default[Option[T]] = new Default[Option[T]] {
        override def default: Option[T] = None
      }
      override def mapInterpret[A, B](fa: Option[A])(f: A => B)(implicit instance: Default[B]): B =
        fa.fold(instance.default)(f)
    }
}
