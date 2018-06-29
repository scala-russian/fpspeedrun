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

  // TODO create the real FreeCalc
  type Expr[A] = List[(Int, List[A])] //Sum of products

  //TODO implement this
  implicit val freeCalc: FreeConstruct[Calc, Expr] =
    new FreeConstruct[Calc, Expr] {
      override def embed[T](x: T): Expr[T] = List(1 -> List(x))
      override def instance[T]: Calc[Expr[T]] = new Calc[Expr[T]] {
        override def fromInt(x: Int): Expr[T] = List(x -> Nil)
        override def plus(x: Expr[T], y: Expr[T]): Expr[T] = x ::: y
        override def times(x: Expr[T], y: Expr[T]): Expr[T] =
          for {
            (k1, p1) <- x
            (k2, p2) <- y
          } yield {
            (k1 * k2) -> (p1 ::: p2)
          }
      }
      override def mapInterpret[A, B](fa: Expr[A])(f: A => B)(implicit instance: Calc[B]): B =
        fa.foldLeft(instance.zero) {
          case (acc, (k, p)) => acc + (instance.fromInt(k) * p.map(f).foldLeft(instance.one)(instance.times))
        }
    }
}

trait StdCalcInstances[TC[typ] >: Calc[typ]] extends StdNumInstances[TC]
