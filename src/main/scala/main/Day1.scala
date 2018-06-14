package main

import fpspeedrun.Ord.Compare
import fpspeedrun.Ratio
import fpspeedrun.syntax.eq._
import fpspeedrun.syntax.ord._
import fpspeedrun.Ratio._

object Day1 extends App {
  assert(Ratio(1, 2) === Ratio(1, 2))
  assert(!(Ratio(1, 2) === Ratio(1, 3)))
  assert(Seq(Ratio(1, 2), Ratio(1, 3)) === Seq(Ratio(1, 2), Ratio(1, 3)))
  assert(!(Seq(Ratio(1, 2), Ratio(1, 3)) === Seq(Ratio(1, 2), Ratio(1, 4))))
}

object Day1Homework extends App {
  assert((Ratio(1, 2) <> Ratio(2, 3)) == Compare.LT)
  assert((Ratio(5, 4) <> Ratio(2, 3)) == Compare.GT)
  assert((Ratio(5, 4) <> Ratio(5, 4)) == Compare.EQ)

  assert((Seq(Ratio(1, 2), Ratio(2, 3)) <> Seq(Ratio(1, 2), Ratio(2, 4))) == Compare.GT)
  assert((Seq(Ratio(1, 2), Ratio(2, 4)) <> Seq(Ratio(1, 2), Ratio(2, 3))) == Compare.LT)

  assert((Seq(Ratio(1, 2), Ratio(2, 3), Ratio(2, 3)) <> Seq(Ratio(1, 2), Ratio(2, 3))) == Compare.GT)
  assert((Seq(Ratio(1, 2), Ratio(2, 3)) <> Seq(Ratio(1, 2), Ratio(2, 3), Ratio(2, 3))) == Compare.LT)
  assert((Seq(Ratio(1, 2), Ratio(2, 3)) <> Seq(Ratio(1, 2), Ratio(2, 3))) == Compare.EQ)
}
