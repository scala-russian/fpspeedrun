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

  type Degree = Int
  type Coefficient = Int

  case class Term[A](x: A, degree: Degree)

  case class Monomial[A](coef: Coefficient, terms: List[Term[A]]) {
    def *(num: Int): Monomial[A] = Monomial(coef * num, terms)

    def *(term: Term[A]): Monomial[A] =
      if (term.degree <= 0) Monomial(coef, terms)
      else {
        val merged = terms.partition(_.x == term.x) match {
          case (Nil, _)      => term :: terms
          case (List(t), ts) => Term(t.x, t.degree + term.degree) :: ts
        }

        Monomial(coef, merged)
      }

    def *(that: Monomial[A]): Monomial[A] =
      that.terms.foldLeft(Monomial(coef * that.coef, terms))(_ * _)

    def ~(that: Monomial[A]): Boolean = terms.toSet == that.terms.toSet

    def ===(that: Monomial[A]): Boolean = (this ~ that) && (coef == that.coef)
  }

  object Monomial {
    def apply[A](coef: Coefficient, terms: List[Term[A]]): Monomial[A] = {
      if (coef == 0) new Monomial(0, Nil)
      else {
        val normalized =
          terms
            .filter(_.degree > 0)
            .groupBy(_.x)
            .map { case (x, list) => Term(x, list.map(_.degree).sum) }
            .toList

        new Monomial(coef, normalized)
      }
    }

    /*implicit def monomialOrd[A]: Ord[Monomial[A]] =
      (a: Monomial[A], b: Monomial[A]) => {
        val degreesDiff =
          (a.xs.map(_._1) ++ b.xs.map(_._1))
            .toSet
            .map { v: A => a.xs.find(_._1 == v).map(_._2).getOrElse(0) - b.xs.find(_._1 == v).map(_._2).getOrElse(0) }
            .sum

        if (degreesDiff > 0) GT else if (degreesDiff < 0) LT else EQ
      }*/

  }

  case class Polynomial[A](monomials: List[Monomial[A]]) {
    def +(that: Polynomial[A]): Polynomial[A] = Polynomial(monomials ++ that.monomials)

    def *(that: Polynomial[A]): Polynomial[A] =
      Polynomial { for (xms <- this.monomials; yms <- that.monomials) yield xms * yms }

    def ===(that: Polynomial[A]): Boolean = {
      @tailrec def loop(target: List[Monomial[A]], source: List[Monomial[A]]): Boolean = {
        source match {
          case Nil       => target.isEmpty
          case h :: tail => target.partition(_ === h) match {
            case (Nil, _)       => false
            case (_ :: Nil, ts) => loop(ts, tail)
          }
        }
      }

      loop(monomials, that.monomials)
    }
  }

  object Polynomial {
    def apply[A](ms: List[Monomial[A]]): Polynomial[A] = {
      @tailrec def collect(target: List[Monomial[A]], source: List[Monomial[A]]): List[Monomial[A]] = {
        source match {
          case Nil       => target
          case h :: tail => target.partition(_ ~ h) match {
            case (Nil, _)       => collect(h :: target, tail)
            case (t :: Nil, ts) => collect(Monomial(h.coef + t.coef, t.terms) :: ts, tail)
          }
        }
      }

      new Polynomial(collect(Nil, ms).filterNot(_.coef == 0))
    }
  }

  type Expr[A] = Polynomial[A]

  implicit val freeCalc: FreeConstruct[Calc, Expr] =
    new FreeConstruct[Calc, Expr] {
      override def embed[T](x: T): Expr[T] = Polynomial(Monomial(1, Term(x, 1) :: Nil) :: Nil)

      override def instance[T]: Calc[Expr[T]] = new Calc[Expr[T]] {
        override def fromInt(x: Coefficient): Expr[T] = Polynomial(Monomial(x, List[Term[T]]()) :: Nil)

        override def plus(x: Expr[T], y: Expr[T]): Expr[T] = x + y

        override def times(x: Expr[T], y: Expr[T]): Expr[T] = x * y
      }

      override def mapInterpret[A, B](fa: Expr[A])(f: A => B)(implicit instance: Calc[B]): B =
        fa.monomials.map { m =>
            instance.fromInt(m.coef) *
            m.terms.map { t => (f(t.x), t.degree) }
              .foldLeft(instance.one)((acc, e) => acc * instance.pow(e._1, e._2))
        }.fold(instance.zero)(instance.plus)
    }
}

trait StdCalcInstances[TC[typ] >: Calc[typ]] extends StdNumInstances[TC]
