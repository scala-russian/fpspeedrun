package fpspeedrun

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import Calc._

class FreeNumSuite extends WordSpec with Matchers with PropertyChecks {

  val instance: Calc[Expr[String]] = implicitly[FreeConstruct[Calc, Expr]].instance[String]

  "ring axioms matching" when {
    "sum commutativity" should {
      "on constants" in {
        val a = Polynomial(List(Monomial(5, List[Term[String]]())))
        val b = Polynomial(List(Monomial(10, List[Term[String]]())))
        instance.plus(a, b) === instance.plus(b, a) shouldBe true
      }
      "on one-element monoms" in {
        val a = Polynomial(List(Monomial(5, Term("hello", 2) :: Nil)))
        val b = Polynomial(List(Monomial(10, Term("Bro", 3) :: Nil)))
        instance.plus(a, b) === instance.plus(b, a) shouldBe true
      }
      "on arbitrary polynomials" in {
        forAll((l: List[(Int, List[(String, Int)])], r: List[(Int, List[(String, Int)])]) => {
          val a = Polynomial(l.map { case (c, m) => Monomial(c, m.map(t => Term(t._1, t._2))) })
          val b = Polynomial(r.map { case (c, m) => Monomial(c, m.map(t => Term(t._1, t._2))) })
          instance.plus(a, b) === instance.plus(b, a) shouldBe true
        })
      }
    }
    "sum assotiativity" should {
      "on constants" in {
        val a = Polynomial(List(Monomial(5, List[Term[String]]())))
        val b = Polynomial(List(Monomial(10, List[Term[String]]())))
        val c = Polynomial(List(Monomial(15, List[Term[String]]())))
        instance.plus(instance.plus(a, b), c) === instance.plus(a, instance.plus(b, c)) shouldBe true
      }
      "on one-element monoms" in {
        val a = Polynomial(List(Monomial(5, Term("hello", 2) :: Nil)))
        val b = Polynomial(List(Monomial(10, Term("Bro", 3) :: Nil)))
        val c = Polynomial(List(Monomial(15, Term("ButerBro", 5) :: Nil)))
        instance.plus(instance.plus(a, b), c) === instance.plus(a, instance.plus(b, c)) shouldBe true
      }
      "on arbitrary polynomials" in {
        forAll((s1: List[(Int, List[(String, Int)])], s2: List[(Int, List[(String, Int)])], s3: List[(Int, List[(String, Int)])]) => {
          val a = Polynomial(s1.map { case (coef, m) => Monomial(coef, m.map(t => Term(t._1, t._2))) })
          val b = Polynomial(s2.map { case (coef, m) => Monomial(coef, m.map(t => Term(t._1, t._2))) })
          val c = Polynomial(s3.map { case (coef, m) => Monomial(coef, m.map(t => Term(t._1, t._2))) })
          instance.plus(instance.plus(a, b), c) === instance.plus(a, instance.plus(b, c)) shouldBe true
        })
      }
    }
    "zero element" should {
      "on constants" in {
        val a = Polynomial(List(Monomial(5, List[Term[String]]())))
        instance.plus(instance.zero, a) === instance.plus(a, instance.zero) shouldBe true
        instance.plus(instance.zero, a) === a shouldBe true
        instance.plus(a, instance.zero) === a shouldBe true
      }
      "on one-element monoms" in {
        val a = Polynomial(List(Monomial(5, Term("hello", 2) :: Nil)))
        instance.plus(instance.zero, a) === instance.plus(a, instance.zero) shouldBe true
        instance.plus(instance.zero, a) === a shouldBe true
        instance.plus(a, instance.zero) === a shouldBe true
      }
      "on arbitrary polynomials" in {
        forAll((s1: List[(Int, List[(String, Int)])]) => {
          val a = Polynomial(s1.map { case (coef, m) => Monomial(coef, m.map(t => Term(t._1, t._2))) })
          instance.plus(instance.zero, a) === instance.plus(a, instance.zero) shouldBe true
          instance.plus(instance.zero, a) === a shouldBe true
          instance.plus(a, instance.zero) === a shouldBe true
        })
      }
    }
  }
}
