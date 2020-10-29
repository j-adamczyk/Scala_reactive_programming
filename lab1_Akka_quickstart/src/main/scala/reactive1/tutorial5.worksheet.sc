
// Why implicits are such a big deal in scala?
// Example - Type Classes - adding some ability without modifying the original class


// operation definition
trait Add[A] {
  def add(x: A, y: A): A
}

// just to enable nice "+" syntax
implicit class Adding[A](x: A)(implicit s: Add[A]) {
  def +(y: A): A =
    s.add(x, y)
}

class Rational(val number: Int, val denom: Int = 1) {
  override def toString = number + "/" + denom
}

object Rational {
  // scala compiler will look into companion object when trying to find missing implicit
  implicit val addRational: Add[Rational] = (x: Rational, y: Rational) => {
    new Rational(x.number * y.denom + y.number * x.denom, x.denom * y.denom)
  }
}

val rational1 = new Rational(1, 2)
val rational2 = new Rational(1, 3)
rational1 + rational2

// some other example
class Basket(val items: Set[String]) {
  override def toString = items.mkString(",")
}

object Basket {
  implicit val addBasket: Add[Basket] = (x: Basket, y: Basket) => new Basket(x.items ++ y.items)
}

val basket1 = new Basket(Set("bicycle"))
val basket2 = new Basket(Set("rollerblades"))
basket1 + basket2

// typical usage in method
def sum[A](a: A, b: A)(implicit adder: Add[A]): A =
  adder.add(a, b)

// above sum can be also written with possibly nicer syntax
// def implicitly[T](implicit e: T): T = e
def sum2[A: Add](a: A, b: A): A = {
  implicitly[Add[A]].add(a, b)
}

sum(rational1, rational2)
sum2(basket1, basket2)

// You can find those type classes in the standard scala library:

implicit object RationalOrdering extends Ordering[Rational] {
  def compare(x: Rational, y: Rational) = {
    // transform to floats, compare floats
    x.number.toFloat / x.denom compare y.number.toFloat / y.denom
  }
}
List(rational1, rational2).sorted

implicit object BasketOrdering extends Ordering[Basket] {
  def compare(x: Basket, y: Basket) = x.items.size compare y.items.size
}

List(basket1, basket2).sorted
