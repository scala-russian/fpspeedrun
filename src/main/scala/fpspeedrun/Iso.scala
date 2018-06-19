package fpspeedrun

trait Iso[A, B] {
  def wrap(a: A): B
  def unwrap(b: B): A
}

object Iso {
  trait Wrapper[T] extends Any {
    def value: T
  }

  trait WrapperCompanion[F[x] <: Wrapper[x]] {
    def apply[T](x: T): F[T]
    implicit def wrapperIso[T]: Iso[T, F[T]] = new Iso[T, F[T]] {
      override def wrap(a: T): F[T]   = apply(a)
      override def unwrap(b: F[T]): T = b.value
    }
  }
}
