package fpspeedrun
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks
import syntax.ord._
import cats.syntax.option._
import fpspeedrun.Ord.Compare.{LT, GT}

class OrdSuite extends WordSpec with Matchers with PropertyChecks {
  "Option comparison" should {
    "make None smallest element" in {
      forAll((x: Int) => (none[Int] compare x.some) shouldBe LT)
    }
    "check somes " in {
      forAll((x: Int, y: Int) => (x.some compare y.some) shouldBe (x compare y))
    }
  }

  "ZipList comparison" should {
    "make smallest repeated smallest ziplist" in {
      forAll((xs: List[Double]) =>
        (ZipList.repeat(0.0) compare ZipList(xs.map(_.abs + 1))) shouldBe LT
      )
    }

    "make larges repeated largest ziplist" in {
      forAll((xs: List[Double]) =>
        (ZipList.repeat(0.0) compare ZipList(xs.map(-_.abs - 1))) shouldBe GT
      )
    }

    "check simple list inequalities" in {
      forAll((xs: List[Int], ys: List[Int]) =>
        (ZipList(xs) compare ZipList(ys)) shouldBe (xs compare ys)
      )
    }

    "check repeated inequalities" in {
      forAll((x: Int, y: Int) =>
        (ZipList.repeat(x) compare ZipList.repeat(y)) shouldBe (x compare y)
      )
    }
  }
}
