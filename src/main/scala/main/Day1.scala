package main

import fpspeedrun.Ratio
import fpspeedrun.syntax.eq._
import fpspeedrun.Ratio._

object Day1 extends App {
  assert(Ratio(1, 2) === Ratio(1, 2))
  assert(!(Ratio(1, 2) === Ratio(1, 3)))
  assert(Seq(Ratio(1, 2), Ratio(1, 3)) === Seq(Ratio(1, 2), Ratio(1, 3)))
  assert(!(Seq(Ratio(1, 2), Ratio(1, 3)) === Seq(Ratio(1, 2), Ratio(1, 4))))
}

object Day1Homework extends App {

}
