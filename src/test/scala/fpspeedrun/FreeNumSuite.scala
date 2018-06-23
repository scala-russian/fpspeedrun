package fpspeedrun

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import Calc._

class FreeNumSuite extends WordSpec with Matchers with PropertyChecks {

  val instance: Calc[Expr[String]] = implicitly[FreeConstruct[Calc, Expr]].instance[String]

  "ring axioms matching" when {
    "commutativity" should {
      "on constants" in {
        val a = Map(List[String]() -> 5)
        val b = Map(List[String]() -> 10)
        instance.plus(a, b) == instance.plus(b, a) shouldBe true
      }
      "on one-element monoms" in {
        val a = Map(List("Hello") -> 5)
        val b = Map(List("Bro") -> 10)
        instance.plus(a, b) == instance.plus(b, a) shouldBe true
      }
      "on arbitrary polynomials" in {
        forAll((a: Map[List[String], Int], b: Map[List[String], Int]) => {
          instance.plus(a, b) == instance.plus(b, a) shouldBe true
        })
      }
    }
    "assotiativity" should {
      "on constants" in {
        val a = Map(List[String]() -> 5)
        val b = Map(List[String]() -> 10)
        val c = Map(List[String]() -> 15)
        instance.plus(instance.plus(a, b), c) == instance.plus(a, instance.plus(b, c)) shouldBe true
      }
      "on one-element monoms" in {
        val a = Map(List("Hello") -> 5)
        val b = Map(List("Bro") -> 10)
        val c = Map(List("Hi") -> 15)
        instance.plus(instance.plus(a, b), c) == instance.plus(a, instance.plus(b, c)) shouldBe true
      }
      "on arbitrary polynomials" in {
        forAll((a: Map[List[String], Int], b: Map[List[String], Int], c: Map[List[String], Int]) => {
          instance.plus(instance.plus(a, b), c) == instance.plus(a, instance.plus(b, c)) shouldBe true
        })
      }
    }
    "zero element" should {
      "on constants" in {
        val a = Map(List[String]() -> 5)
        instance.plus(instance.zero, a) == instance.plus(a, instance.zero) shouldBe true
        instance.plus(instance.zero, a) == a shouldBe true
        instance.plus(a, instance.zero) == a shouldBe true
      }
      "on one-element monoms" in {
        val a = Map(List("Hello") -> 5)
        instance.plus(instance.zero, a) == instance.plus(a, instance.zero) shouldBe true
        instance.plus(instance.zero, a) == a shouldBe true
        instance.plus(a, instance.zero) == a shouldBe true
      }
      "on arbitrary polynomials" in {
        forAll((a: Map[List[String], Int]) => {
          instance.plus(instance.zero, a) == instance.plus(a, instance.zero) shouldBe true
          instance.plus(instance.zero, a) == a shouldBe true
          instance.plus(a, instance.zero) == a shouldBe true
        })
      }
    }
  }
}
