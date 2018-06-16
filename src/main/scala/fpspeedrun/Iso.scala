package fpspeedrun

trait Iso[T, U] {
  def wrap(x: T): U
  def unwrap(x: U): T
}
