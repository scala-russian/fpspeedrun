package fpspeedrun

import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks
import syntax.semigroup._
import syntax.ratio._
import syntax.num._

class SemigroupSuite extends WordSpec with Matchers with PropertyChecks {
  "list optional reducing" when {
    "reducing strings" should {
      "reduce empty list" in {
        List.empty[String].reduceOpt shouldBe None
      }
      "reduce non empty list" in {
        forAll((x : String, xs: List[String]) =>
          (x :: xs).reduceOpt shouldBe Some((x :: xs).mkString))
      }
    }

    "reducing ints" when {
      "adding" should {
        "reduce empty list" in {
          List.empty[Int].reduceOptVia[Sum] shouldBe None
        }
        "reduce non-empty list" in {
          forAll((x: Int, xs: List[Int]) =>
            (x :: xs).reduceOptVia[Sum] shouldBe Some(x + xs.sum))
        }
      }

      "multiplying" should {
        "reduce empty list" in {
          List.empty[Int].reduceOptVia[Prod] shouldBe None
        }
        "reduce non-empty list" in {
          forAll((x: Int, xs: List[Int]) =>
            (x :: xs).reduceOptVia[Prod] shouldBe Some(x * xs.product))
        }
      }

      "searching leftmost" should {
        "reduce empty list" in {
          List.empty[Int].reduceOptVia[First] shouldBe None
        }
        "reduce non-empty list" in {
          forAll((x: Int, xs: List[Int]) =>
            (x :: xs).reduceOptVia[First] shouldBe Some(x))
        }
      }

      "searching rightmost" should {
        "reduce empty list" in {
          List.empty[Int].reduceOptVia[Last] shouldBe None
        }
        "reduce non-empty list" in {
          forAll((x: Int, xs: List[Int]) =>
            (x :: xs).reduceOptVia[Last] shouldBe Some((x :: xs).last))
        }
      }
    }

    "reducing ratios" when {
      "fractions are integers" should {
        "sum numerators" in {
          forAll((xs: List[Int]) =>
            xs.map(_.toRatio).reduceOptVia[Sum] shouldBe xs.reduceOption(_ + _).map(_.toRatio)
          )
        }

        "multiply numerators" in {
          forAll((xs: List[Int]) =>
            xs.map(_.toRatio).reduceOptVia[Prod] shouldBe xs.reduceOption(_ * _).map(_.toRatio)
          )
        }
      }
      "facing arbitrary fractions" should {
        "sum fractions" in {
          forAll((xs: List[Ratio[BigInt]]) =>
            xs.reduceOptVia[Sum] shouldBe xs.reduceOption(_ + _)
          )
        }

        "multiply fractions" in {
          forAll((xs: List[Ratio[BigInt]]) =>
            xs.reduceOptVia[Prod] shouldBe xs.reduceOption(_ * _)
          )
        }

        "find leftmost fraction" in {
          forAll((xs: List[Ratio[BigInt]]) =>
            xs.reduceOptVia[First] shouldBe xs.headOption
          )
        }

        "find rightmost fraction" in {
          forAll((xs: List[Ratio[BigInt]]) =>
            xs.reduceOptVia[Last] shouldBe xs.lastOption
          )
        }
      }
    }
  }
}
