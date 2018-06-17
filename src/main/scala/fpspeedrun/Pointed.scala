package fpspeedrun
import simulacrum.typeclass


@typeclass
trait Pointed[A] {
  def empty: A
}
