package fpspeedrun
import org.scalacheck.Arbitrary
import Arbitrary.arbitrary
import syntax.ratio._

trait RatioGenerators {
  implicit def arbitraryRatio[T: Arbitrary: Integ]: Arbitrary[Ratio[T]] =
    Arbitrary(for {
      numerator   <- arbitrary[T]
      denominator <- arbitrary[T]
    } yield numerator \\ denominator)
}
