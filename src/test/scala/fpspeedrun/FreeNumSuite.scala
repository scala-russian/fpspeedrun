package fpspeedrun

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import Calc._

class FreeNumSuite extends WordSpec with Matchers with PropertyChecks {

  val instance: Calc[Expr[String]] = implicitly[FreeConstruct[Calc, Expr]].instance[String]

  "ring axioms matching" when {
    "sum commutativity" should {
      "on constants" in {
        val a = Polynomial(List(Monomial(5, List[(String, Int)]())))
        val b = Polynomial(List(Monomial(10, List[(String, Int)]())))
        instance.plus(a, b) == instance.plus(b, a) shouldBe true
      }
      "on one-element monoms" in {
        val a = Polynomial(List(Monomial(5, ("hello", 2) :: Nil)))
        val b = Polynomial(List(Monomial(10, ("Bro", 3) :: Nil)))
        instance.plus(a, b) == instance.plus(b, a) shouldBe true
      }
      "on arbitrary polynomials" in {
        forAll((l: List[(Int, List[(String, Int)])], r: List[(Int, List[(String, Int)])]) => {
          val a = Polynomial(l.map { case (c, m) => Monomial(c, m) })
          val b = Polynomial(r.map { case (c, m) => Monomial(c, m) })
          instance.plus(a, b) == instance.plus(b, a) shouldBe true
        })
      }
    }
    "sum assotiativity" should {
      "on constants" in {
        val a = Polynomial(List(Monomial(5, List[(String, Int)]())))
        val b = Polynomial(List(Monomial(10, List[(String, Int)]())))
        val c = Polynomial(List(Monomial(15, List[(String, Int)]())))
        instance.plus(instance.plus(a, b), c) == instance.plus(a, instance.plus(b, c)) shouldBe true
      }
      "on one-element monoms" in {
        val a = Polynomial(List(Monomial(5, ("hello", 2) :: Nil)))
        val b = Polynomial(List(Monomial(10, ("Bro", 3) :: Nil)))
        val c = Polynomial(List(Monomial(15, ("Bro", 5) :: Nil)))
        instance.plus(instance.plus(a, b), c) == instance.plus(a, instance.plus(b, c)) shouldBe true
      }
      "on arbitrary polynomials" in {
        forAll((l: List[(Int, List[(String, Int)])], m: List[(Int, List[(String, Int)])], r: List[(Int, List[(String, Int)])]) => {
          val a = Polynomial(l.map { case (coef, xs) => Monomial(coef, xs) })
          val b = Polynomial(m.map { case (coef, xs) => Monomial(coef, xs) })
          val c = Polynomial(r.map { case (coef, xs) => Monomial(coef, xs) })
          instance.plus(instance.plus(a, b), c) == instance.plus(a, instance.plus(b, c)) shouldBe true
        })
      }
    }
    "zero element" should {
      "on constants" in {
        val a = Polynomial(List(Monomial(5, List[(String, Int)]())))
        instance.plus(instance.zero, a) == instance.plus(a, instance.zero) shouldBe true
        instance.plus(instance.zero, a) == a shouldBe true
        instance.plus(a, instance.zero) == a shouldBe true
      }
      "on one-element monoms" in {
        val a = Polynomial(List(Monomial(5, ("hello", 2) :: Nil)))
        instance.plus(instance.zero, a) == instance.plus(a, instance.zero) shouldBe true
        instance.plus(instance.zero, a) == a shouldBe true
        instance.plus(a, instance.zero) == a shouldBe true
      }
      "on arbitrary polynomials" in {
        forAll((l: List[(Int, List[(String, Int)])]) => {
          val a = Polynomial(l.map { case (coef, xs) => Monomial(coef, xs) })
          instance.plus(instance.zero, a) == instance.plus(a, instance.zero) shouldBe true
          instance.plus(instance.zero, a) == a shouldBe true
          instance.plus(a, instance.zero) == a shouldBe true
        })
      }
    }
  }
}
