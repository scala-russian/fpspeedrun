package fpspeedrun.syntax

import fpspeedrun.Ord
import fpspeedrun.Ord.Compare

object listsOrd {
  import ord._

  implicit def compareList[T: Ord]: Ord[List[T]] = (x: List[T], y: List[T]) => {
    val result = x.zip(y).collectFirst {
      case (x1, x2) if x1.compare(x2) != Compare.EQ => x1 compare x2
    }.getOrElse(Compare.EQ)
    if (result != Compare.EQ || x.length == y.length) result
    else if (x.length < y.length) Compare.LT
    else Compare.GT
  }
}
