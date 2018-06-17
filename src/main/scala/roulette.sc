import cats.data.StateT
import cats.effect.IO
import cats.syntax.applicative._
import cats.syntax.apply._

import scala.util.Random

val lastDJSpiewakTweet =
  """tldr: Sometimes, Stream#map in fs2 1.0.0-M can realign chunk boundaries. This is a break in behavior from 0.10 that can cause significant performance regression."""
    .stripMargin

val studentName = "@\\w+".r

val mainCount = 1
val reserveCount = 2

val students = studentName.findAllIn(
  """
    |@ybogomolov, @odbc1986, @ginger_bread_man,
    |@prinstonsam,  @DenisKormalev,  @valekseev
  """
).toArray.sorted

val seed = lastDJSpiewakTweet.##

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

val secured = List("@yanTarakan", "@katerina_gl")
val chosen = chooseAll.unsafeRunSync()

println(
  s"""
     |main   : ${(secured ++ chosen.take(mainCount)).mkString(", ")}
     |reserve: ${chosen.drop(mainCount).mkString(", ")}
  """.stripMargin)