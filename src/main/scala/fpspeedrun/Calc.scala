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
  import syntax.eq._
  import syntax.ord._

  def fastPow[A](x: A, p: Int)(implicit calc: Calc[A]): A = {
    @tailrec def go(p2: A, acc: A, p: Int): A =
      if (p == 0) acc
      else go(p2 * p2, if (p % 2 == 0) acc else acc * p2, p / 2)

    go(x, calc.one, p)
  }

  type Degree = Int
  type Coefficient = Int

  case class Monomial[A](coef: Coefficient, xs: List[(A, Degree)]) {
    def *(num: Int): Monomial[A] = Monomial(coef * num, xs)

    def *(x: (A, Degree)): Monomial[A] =
      Monomial(
        coef,
        if (xs.exists(_._1 == x._1)) xs.map { e => if (e._1 == x._1) (e._1, e._2 + x._2) else e }
        else x :: xs
      )

    def *(that: Monomial[A]): Monomial[A] =
      that.xs.foldLeft(Monomial(this.coef * that.coef, List[(A, Degree)]()))(_ * _)
  }

  object Monomial {
    def apply[A](coef: Coefficient, xs: List[(A, Degree)]): Monomial[A] =
      new Monomial(coef, if (coef == 0) List[(A, Degree)]() else xs.filter(_._2 > 0))

    implicit def monomialOrd[A]: Ord[Monomial[A]] =
      (a: Monomial[A], b: Monomial[A]) => {
        val degreesDiff =
          (a.xs.map(_._1) ++ b.xs.map(_._1))
            .toSet
            .map { v: A => a.xs.find(_._1 == v).map(_._2).getOrElse(0) - b.xs.find(_._1 == v).map(_._2).getOrElse(0) }
            .sum

        if (degreesDiff > 0) GT else if (degreesDiff < 0) LT else EQ
      }

  }

  case class Polynomial[A](monomials: List[Monomial[A]])(implicit mOrd: Ord[Monomial[A]]) {
    def +(that: Polynomial[A]): Polynomial[A] = Polynomial(monomials ++ that.monomials)

    def *(that: Polynomial[A]): Polynomial[A] =
      Polynomial { for (xms <- this.monomials; yms <- that.monomials) yield xms * yms }
  }

  object Polynomial {
    def apply[A](m: List[Monomial[A]])(implicit mOrd: Ord[Monomial[A]]): Polynomial[A] =
      new Polynomial(m.filterNot(_.coef == 0).sortWith { _ < _ })
  }

  type Expr[A] = Polynomial[A]

  implicit val freeCalc: FreeConstruct[Calc, Expr] =
    new FreeConstruct[Calc, Expr] {
      override def embed[T](x: T): Expr[T] = Polynomial(Monomial(1, (x, 1) :: Nil) :: Nil)

      override def instance[T]: Num[Expr[T]] = new Num[Expr[T]] {
        override def fromInt(x: Coefficient): Expr[T] = Polynomial(Monomial(x, List[(T, Degree)]()) :: Nil)

        override def plus(x: Expr[T], y: Expr[T]): Expr[T] = x + y

        override def times(x: Expr[T], y: Expr[T]): Expr[T] = x * y

        override def compare(x: Expr[T], y: Expr[T]): Ord.Compare =
          (x.monomials zip y.monomials).collectFirst { case (a, b) if (a <=> b) =/= EQ => a <=> b }
            .getOrElse(x.monomials.size <=> y.monomials.size)
      }

      override def mapInterpret[A, B](fa: Expr[A])(f: A => B)(implicit instance: Calc[B]): B =
        fa.monomials.map { m =>
            instance.fromInt(m.coef) *
            m.xs.map { case (t, d) => (f(t), d) }
              .foldLeft(instance.one)((acc, e) => acc * instance.pow(e._1, e._2))
        }.fold(instance.zero)(instance.plus)
    }
}

trait StdCalcInstances[TC[typ] >: Calc[typ]] extends StdNumInstances[TC]
