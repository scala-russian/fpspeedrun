package fpspeedrun
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import cats.syntax.either._
import cats.syntax.option._
import syntax.eq._

class EqSuite extends WordSpec with Matchers with PropertyChecks {
  "option equality" should {
    "verify equality of nones" in (
      (none[Int] equal none[Int]) shouldBe true
    )

    "verify equality of somes" in {
      forAll((x: Int, y: Int) => (x.some equal y.some) shouldBe (x equal y))
    }

    "check equality of constructors" in {
      forAll { x: Int =>
        (x.some equal none[Int]) shouldBe false
        (none[Int] equal x.some) shouldBe false
      }
    }
  }

  "either equality" should {
    "verify equality of lefts" in
      forAll((x: Int, y: Int) => (x.asLeft[String] equal y.asLeft[String]) shouldBe (x equal y))
    "verify equality of rights" in
      forAll((x: String, y: String) => (x.asRight[Int] equal y.asRight[Int]) shouldBe (x equal y))
    "check equality of constructors" in
      forAll((x: Int, y: Int) => (x.asLeft[Int] equal y.asRight[Int]) shouldBe false)
  }
}
