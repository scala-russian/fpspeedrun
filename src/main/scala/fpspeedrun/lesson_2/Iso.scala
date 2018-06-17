package fpspeedrun.lesson_2

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
trait Iso[U, W] {
  def wrap(x: U): W
  def unwrap(x: W): U
}