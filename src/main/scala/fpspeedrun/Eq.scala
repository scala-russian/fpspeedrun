package fpspeedrun

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}
