package fpspeedrun.lesson_2

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
trait SemiGroup[T] {
  def combine(x: T, y: T): T
}

object SemiGroup {
  import fpspeedrun.syntax.semigroup._

  object Laws {
    def associativity[T : SemiGroup](x: T, y: T, z: T): Boolean = {
      ((x |+| y) |+| z) == (x |+| (y |+| z))
    }
  }
}
