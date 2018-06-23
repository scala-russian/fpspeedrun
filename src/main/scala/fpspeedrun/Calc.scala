package fpspeedrun

import fpspeedrun.Ord.Compare.{EQ, GT, LT}
import simulacrum.{op, typeclass}

import scala.annotation.tailrec

@typeclass
trait Calc[A] {
  def fromInt(x: Int): A

  @op("+", alias = true)
  def plus(x: A, y: A): A

  @op(name = "*", alias = true)
  def times(x: A, y: A): A

  def zero: A     = fromInt(0)
  def one: A      = fromInt(1)
  def minusOne: A = fromInt(-1)

  @op("unary_!", alias = true)
  def negate(x: A): A = times(x, minusOne)

  @op("-", alias = true)
  def minus(x: A, y: A): A = plus(x, negate(y))

  def pow(x: A, p: Int): A = Calc.fastPow(x, p)(this)
}

object Calc extends StdCalcInstances[Calc] {
  import ops._

  def fastPow[A](x: A, p: Int)(implicit calc: Calc[A]): A = {
    @tailrec def go(p2: A, acc: A, p: Int): A =
      if (p == 0) acc
      else go(p2 * p2, if (p % 2 == 0) acc else acc * p2, p / 2)

    go(x, calc.one, p)
  }

  type Expr[A] = Map[List[A], Int]  // model of polynomial:
                                    // List[A] - indeterminates non-commutative product,
                                    // Int - coefficient

  implicit val freeCalc: FreeConstruct[Calc, Expr] =
    new FreeConstruct[Calc, Expr] {
      override def embed[T](x: T): Expr[T] = Map(List(x) -> 1)
      override def instance[T]: Num[Expr[T]] = new Num[Expr[T]] {
        private def merge(x: Expr[T], y: Expr[T]): Expr[T] = {
          x ++ y.map { case (k, v) => k -> (v + x.getOrElse(k, 0)) }
        }

        override def compare(x: Expr[T], y: Expr[T]): Ord.Compare = {
          def maxDegree(x: Expr[T]): Int = x.keys.map(_.length).max

          if (x == y) EQ
          else if (maxDegree(x) > maxDegree(y)) GT
          else if (maxDegree(x) < maxDegree(y)) LT
          else EQ  // TODO implement any ordering of polynomials with equal degrees
        }

        override def fromInt(x: Int): Expr[T] = Map(List() -> x)

        override def plus(x: Expr[T], y: Expr[T]): Expr[T] = merge(x, y)

        override def times(x: Expr[T], y: Expr[T]): Expr[T] = (for {
            (mx, cx) <- x
            (my, cy) <- y
          } yield (mx ++ my, cx * cy)).foldLeft(Map[List[T], Int]())((a: Expr[T], b: (List[T], Int)) => merge(a, List(b).toMap))
      }
      override def mapInterpret[A, B](fa: Expr[A])(f: A => B)(implicit instance: Calc[B]): B =
        (fa map {
          case (m, c) =>
            instance.times(m.map(f).fold(instance.one)(instance.times), instance.fromInt(c))
        }).fold(instance.zero)(instance.plus)
    }
}

trait StdCalcInstances[TC[typ] >: Calc[typ]] extends StdNumInstances[TC]
