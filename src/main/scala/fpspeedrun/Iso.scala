package fpspeedrun

trait Iso[A, B] {
  def wrap(a: A): B
  def unwrap(b: B): A
}


object Iso {
  trait Wrapper[T] extends Any {
    def value: T
  }

  trait WrapperIso[T, F[x] <: Wrapper[x]] extends Iso[T, F[T]] {
    override def unwrap(b: F[T]): T = b.value
  }
}

