package fpspeedrun

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

  type Expr[A] = List[(Int, List[A])] // polynomial

  implicit val freeCalc: FreeConstruct[Calc, Expr] =
    new FreeConstruct[Calc, Expr] {
      override def embed[T](x: T): Expr[T] = 1 -> List(x) :: Nil
      override def instance[T]: Calc[Expr[T]] = new Calc[Expr[T]] {
        override def plus(x: Expr[T], y: Expr[T]): Expr[T] = x ::: y
        override def times(x: Expr[T], y: Expr[T]): Expr[T] = for {
          (xc, xp) <- x
          (yc, yp) <- y
        } yield (xc * yc) -> (xp ::: yp)
        override def fromInt(x: Int): Expr[T] = List(x -> Nil)
      }
      override def mapInterpret[A, B](fa: Expr[A])(f: A => B)(implicit instance: Calc[B]): B =
        fa.foldLeft(instance.zero) {
          case (acc, (c, p)) => acc + (instance.fromInt(c) * p.map(f).reduce(_ * _))
        }
    }
}

trait StdCalcInstances[TC[typ] >: Calc[typ]] extends StdNumInstances[TC]
