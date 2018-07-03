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

sealed trait Expr[+A]
final case class IntCoef(c: Int) extends Expr[Nothing]
final case class Value[A](a: A) extends Expr[A]
final case class Plus[A](l: Expr[A], r: Expr[A]) extends Expr[A]
final case class Times[A](l: Expr[A], r: Expr[A]) extends Expr[A]

object Calc extends StdCalcInstances[Calc] {
  import ops._

  def fastPow[A](x: A, p: Int)(implicit calc: Calc[A]): A = {
    @tailrec def go(p2: A, acc: A, p: Int): A =
      if (p == 0) acc
      else go(p2 * p2, if (p % 2 == 0) acc else acc * p2, p / 2)

    go(x, calc.one, p)
  }

  implicit val freeCalc: FreeConstruct[Calc, Expr] =
    new FreeConstruct[Calc, Expr] {
      override def embed[T](x: T): Expr[T] = Value(x)
      override def instance[T]: Calc[Expr[T]] = new Calc[Expr[T]] {
        override def plus(x: Expr[T], y: Expr[T]): Expr[T] = Plus(x, y)
        override def times(x: Expr[T], y: Expr[T]): Expr[T] = Times(x, y)
        override def fromInt(x: Int): Expr[T] = IntCoef(x)
      }
      override def mapInterpret[A, B](fa: Expr[A])(f: A => B)(implicit instance: Calc[B]): B = {
        def go(expr: Expr[A]): B = {
          expr match {
            case IntCoef(c)   => instance.fromInt(c)
            case Value(a)     => f(a)
            case Plus(l, r)   => go(l) + go(r)
            case Times(l, r)  => go(l) * go(r)
          }
        }
        go(fa)
      }
    }
}

trait StdCalcInstances[TC[typ] >: Calc[typ]] extends StdNumInstances[TC]
