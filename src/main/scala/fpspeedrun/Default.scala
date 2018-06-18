package fpspeedrun
import simulacrum.typeclass

/** Pointed type */
@typeclass
trait Default[A] {
  def default: A
}
