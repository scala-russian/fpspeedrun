package fpspeedrun

trait FreeConstruct[Typeclass[_], Construct[_]] {
  def embed[T](x: T): Construct[T]
  def instance[T]: Typeclass[Construct[T]]
  def mapInterpret[A, B](fa: Construct[A])(f: A => B)(implicit instance: Typeclass[B]): B
}
