package fpspeedrun
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import syntax.monoid.{empty => mempty, _}
import syntax.frac._
import syntax.integ._
import syntax.num._
import syntax.ord._
import syntax.eq._

class MonoidSuite extends WordSpec with Matchers with PropertyChecks {
  "requesting empty element" should {
    "yield empty string" in {
      mempty[String] shouldBe ""
    }

    "yield empty list" in {
      mempty[List[Double]] shouldBe List.empty
    }

    "yield empty sum" in {
      mempty[Sum[Double]] shouldBe Sum(0.0)
    }

    "yield empty prod" in {
      mempty[Prod[Double]] shouldBe Prod(1.0)
    }
  }


  "monoid syntax extension for list" when {
    "folding strings" should {
      "concat list" in forAll(
        (xs                      : List[String]) => xs.foldAll shouldBe xs.mkString)
      "concat ints" in forAll(
        (xs                      : List[Int]) => xs.foldMap(_.toString) shouldBe xs.mkString)
    }

    "folding lists" should {
      "concat list" in forAll(
        (xss                     : List[List[String]]) => xss.foldAll shouldBe xss.flatten)
      "concat ints" in forAll(
        (xs                      : List[Int]) => xs.foldMap(List(_)) shouldBe xs)
    }

    "adding big integers" should {
      "calculate foldVia" in forAll(
        (xs                        : List[BigInt]) => xs.foldVia[Sum] shouldBe xs.sum)

      "calculate foldMap" in forAll(
        (xs                      : List[BigInt]) => xs.foldMap(Sum(_)) shouldBe Sum(xs.sum))
    }

    "multiplying big integers" should {
      "calculate foldVia" in forAll(
        (xs                            : List[BigInt]) => xs.foldVia[Prod] shouldBe xs.product)

      "calculate foldMap" in forAll(
        (xs                      : List[BigInt]) => xs.foldMap(Prod(_)) shouldBe Prod(xs.product))
    }

    "combining endos" should {
      "compose big integers transformations" in
        forAll((ops: List[Endo[BigInt]], start: BigInt) =>
          ops.foldAll.run(start) shouldBe ops.foldLeft(start)((x, endo) =>
            endo.run(x)))
    }
  }

  "composing zipLists" should {
    "zip-strict-compare integers" in {
      forAll((xs: List[Int], ys: List[Int]) =>
        (ZipList(xs) equal ZipList(ys)) shouldBe ZipList((xs, ys).zipped.map(_ equal _)))
    }

    "zip-strict-compare strings" in {
      forAll((xs: List[String], ys: List[String]) =>
        (ZipList(xs) equal ZipList(ys)) shouldBe ZipList((xs, ys).zipped.map(_ equal _)))
    }

    "zip-fuzzy-compare integers" in {
      forAll((xs: List[Int], ys: List[Int]) =>
        (ZipList(xs) <=> ZipList(ys)) shouldBe ZipList((xs, ys).zipped.map(_ <=> _)))
    }

    "zip-add integers" in {
      forAll((xs: List[Int], ys: List[Int]) =>
        (ZipList(xs) + ZipList(ys)) shouldBe ZipList((xs, ys).zipped.map(_ + _))
      )
    }

    "zip-multiply integers" in {
      forAll((xs: List[Int], ys: List[Int]) =>
        (ZipList(xs) * ZipList(ys)) shouldBe ZipList((xs, ys).zipped.map(_ * _)))
    }

    "zip-quotient integers" in {
      forAll((xs: List[Int], ys: List[Int]) =>
        (ZipList(xs) / ZipList(ys)) shouldBe ZipList((xs, ys).zipped.map(_ / _)))
    }

    "zip-mod integers" in {
      forAll((xs: List[Int], ys: List[Int]) =>
        (ZipList(xs) % ZipList(ys)) shouldBe ZipList((xs, ys).zipped.map(_ % _)))
    }

    "zip-divide doubles" in {
      forAll((xs: List[Double], ys: List[Double]) =>
        (ZipList(xs) / ZipList(ys)) shouldBe ZipList((xs, ys).zipped.map(_ / _)))
    }
  }
}
