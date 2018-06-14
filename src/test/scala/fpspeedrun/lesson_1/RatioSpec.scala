package fpspeedrun.lesson_1

import fpspeedrun.lesson_1.Ord.Compare
import org.scalatest.{FlatSpec, Matchers}
import fpspeedrun.syntax.eq._
import fpspeedrun.syntax.ord._

/**
  * @author Mikhail Nemenko { @literal <mnemenko@gmail.com>}
  */
class RatioSpec extends FlatSpec with Matchers {

  it should "be equals" in {
    val x = Ratio(1, 2)
    val y = Ratio(1, 2)

    x ==== y shouldBe true
  }

  it should "be equal with the same gcd result" in {
    val x = Ratio(1, 2)
    val y = Ratio(2, 4)

    x ==== y shouldBe true
  }

  it should "not be true" in {
    val x = Ratio(5, 2)
    val y = Ratio(2, 5)

    x ==== y shouldBe false
  }

  it should "be true because both seq have equals Ratio" in {
    val x = Seq(Ratio(1, 2), Ratio(6, 2), Ratio(101, 2))
    val y = Seq(Ratio(1, 2), Ratio(6, 2), Ratio(101, 2))

    x ==== y shouldBe true
  }

  it should "be EQ ordering" in {
    val x = Seq(Ratio(1, 2), Ratio(6, 2), Ratio(101, 2))
    val y = Seq(Ratio(1, 2), Ratio(6, 2), Ratio(101, 2))

    x <> y shouldBe Compare.EQ
  }

  it should "be LT ordering" in {
    val x = Seq(Ratio(2, 2), Ratio(6, 2), Ratio(101, 2))
    val y = Seq(Ratio(1, 2), Ratio(6, 2), Ratio(101, 2))

    x <> y shouldBe Compare.LT
  }

  it should "be GT ordering" in {
    val x = Seq(Ratio(1, 3), Ratio(6, 2), Ratio(101, 2))
    val y = Seq(Ratio(1, 2), Ratio(6, 2), Ratio(101, 2))

    x <> y shouldBe Compare.GT
  }

  it should "be LT ordering because second seq has more elements" in {
    val x = Seq(Ratio(1, 2), Ratio(6, 2), Ratio(101, 2))
    val y = Seq(Ratio(1, 2), Ratio(6, 2), Ratio(101, 2), Ratio(101, 2))

    x <> y shouldBe Compare.LT
  }

  it should "be GT ordering because first seq has more elements" in {
    val x = Seq(Ratio(1, 2), Ratio(6, 2), Ratio(101, 2), Ratio(101, 2))
    val y = Seq(Ratio(1, 2), Ratio(6, 2), Ratio(101, 2))

    x <> y shouldBe Compare.GT
  }
}
