import cats.data.StateT
import cats.effect.IO
import cats.syntax.applicative._
import cats.syntax.apply._

import scala.util.Random

val lastJDegoesTweet =
  """It’s growing fast. More Haskell jobs right now than at any other point in history."""
    .stripMargin

val studentName = "@\\w+".r

val mainCount = 3
val reserveCount = 2

val students = studentName.findAllIn(
  """
    @kurlov, @ybogomolov, @forzaken, @prinstonsam,
    @nornic,@IceTFoer , @eniqen, @loskin
    @imielientiev, @talys, @guga4ka, @odbc1986,
    @igorz, @talys, @yanTarakan, @MikhailLacksh
  """
).toArray.sorted

val seed = lastJDegoesTweet.##

type Pool = IndexedSeq[String]
type Roulette[a] = StateT[IO, Pool, a]

def chooseOne: Roulette[String] =
  for {
    current <- StateT.get[IO,Pool]
    idx <- StateT.liftF(IO(Random.nextInt(current.size)))
    elem = current(idx)
    nextColl = current.take(idx) ++ current.drop(idx + 1)
    _ <- StateT.set[IO, Pool](nextColl)
  } yield elem

val chooseAll =
  IO(Random.setSeed(seed)) *>
    chooseOne
      .replicateA(mainCount + reserveCount)
      .runA(students)

val chosen = chooseAll.unsafeRunSync()

println(
  s"""
     |main   : ${chosen.take(mainCount).mkString(", ")}
     |reserve: ${chosen.drop(mainCount).mkString(", ")}
  """.stripMargin)