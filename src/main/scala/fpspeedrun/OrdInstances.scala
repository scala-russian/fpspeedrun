package fpspeedrun

import fpspeedrun.Ord.{EQ, GT, LT}
import syntax.ord.OrdOps

object OrdInstances {

  implicit def sizeFirstListOrd[T : Ord]: Ord[List[T]] = (x: List[T], y: List[T]) =>
    if (x.size < y.size) LT
    else if (x.size > y.size) GT
    else x zip y find { case (a, b) => a !== b } match {
      case Some((a, b)) => a compare b
      case None         => EQ
    }

  implicit def sizeOnlyListOrd[T : Ord]: Ord[List[T]] = (x: List[T], y: List[T]) =>
    if (x.size < y.size) LT
    else if (x.size > y.size) GT
    else EQ

}
