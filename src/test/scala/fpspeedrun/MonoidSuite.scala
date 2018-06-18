package fpspeedrun
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import syntax.monoid._

class MonoidSuite extends WordSpec with Matchers with PropertyChecks {
  "monoid syntax" when {
    "folding strings" should{
      "concat list" in forAll((xs: List[String]) =>
        xs.foldAll shouldBe xs.mkString
      )
    }
  }
}
