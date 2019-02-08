package fpspeedrun.functors
import cats.Functor

sealed trait HListK[A]
final case class HNilK[A]() extends HListK[A]
final case class ::#[H[_], T[x] <: HListK[x], A](head: H[A], tail: T[A]) extends HListK[A]

object HListK {
  implicit val hnilFunctor: Functor[HNilK] = ???
  implicit def hconsFunctor[F[_] : Functor, T[x] <: HListK[x] : Functor]: Functor[F ::# T] = ???
}
