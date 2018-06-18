package fpspeedrun
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import cats.syntax.either._
import syntax.eq._

class EqSuite extends WordSpec with Matchers with PropertyChecks {
  "either equality" should {
    "verify equality of lefts" in
      forAll((x: Int, y: Int) =>
        (x.asLeft[String] equal y.asLeft[String]) shouldBe (x equal y))
    "verify equality of rights" in
      forAll((x: String, y: String) =>
        (x.asRight[Int] equal y.asRight[Int]) shouldBe (x equal y))
    "check equality of constructors" in
      forAll((x: Int, y: Int) =>
        (x.asLeft[Int] equal y.asRight[Int]) shouldBe false)
  }
}
