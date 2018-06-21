import fpspeedrun.Ratio
import fpspeedrun.syntax.eq._
import fpspeedrun.syntax.ord._


val r1 = Ratio(1, 2)
val r2 = Ratio(2, 4)
val r3 = Ratio(1, 3)
val r4 = Ratio(3, 6)
val r5 = Ratio(4, 8)

r1 === r2
r1 === r3

r1 <> r2
r1 <> r3

List(r1, r2) === List(r4, r3)
List(r1, r2) === List(r4, r5)

List(r1, r2) <> List(r4, r3)
List(r1, r2) <> List(r4, r5)
List(r3, r2) <> List(r4, r5)
List(r3) <> List(r4, r5)
List(r3, r2) <> List(r4)
List(r1, r2) <> List(r4)
