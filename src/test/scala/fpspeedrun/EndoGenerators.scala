package fpspeedrun
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import syntax.num._
import syntax.integ._
import syntax.eq._

trait EndoGenerators {
  implicit def arbitraryEndo[T: Arbitrary : Integ]: Arbitrary[Endo[T]] =
    Arbitrary(for {
      x <- arbitrary[T]
      action <-
        if (x === zero) Gen.oneOf((_: T) + x, (_: T) * x)
        else Gen.oneOf((_: T) + x, (_: T) * x, (_: T) / x, (_: T) % x)
    } yield Endo(action))
}
