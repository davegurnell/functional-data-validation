/*
In this sprint we create the basic building blocks for
chaining rules in sequence.

We need a way of passing values from one rule to another,
so we add a type parameter to `Result` and introduce a new
implementation of `and` so we can combine results together.
*/
object Main extends App {

  // Validation library -------------------------

  sealed trait Result[+A] {
    def and[B, C](that: Result[B])(func: (A, B) => C) = (this, that) match {
      case (Pass(a), Pass(b)) => Pass(func(a, b))
      case (Fail(a), Pass(b)) => Fail(a)
      case (Pass(a), Fail(b)) => Fail(b)
      case (Fail(a), Fail(b)) => Fail(a ++ b)
    }

    def map[B](func: A => B) = this match {
      case Pass(a) => Pass(func(a))
      case Fail(a) => Fail(a)
    }

    def flatMap[B](func: A => Result[B]) = this match {
      case Pass(a) => func(a)
      case Fail(a) => Fail(a)
    }
  }

  final case class Pass[A](value: A) extends Result[A]
  final case class Fail(messages: List[String]) extends Result[Nothing]

  type Rule[A] = A => Result[A]

  // Application code ---------------------------

  case class Address(number: Int, street: String)

  def nonEmpty(str: String): Result[String] =
    if(str.isEmpty) Fail(List("Empty string")) else Pass(str)

  def initialCap(str: String): Result[String] =
    if(str(0).isUpper) Pass(str) else Fail(List("No initial cap"))

  def capitalize(str: String): String =
    str(0).toUpper +: str.substring(1)

  def gte(min: Int): Rule[Int] =
    (num: Int) =>
      if(num < min) Fail(List("Too small")) else Pass(num)

  def checkStreet(str: String): Result[String] =
    nonEmpty(str).flatMap(initialCap)
    // or nonEmpty(str).map(capitalize)

  def checkAddress(addr: Address) = {
    val numberResult = gte(1)(addr.number)
    val streetResult = checkStreet(addr.street)

    numberResult.and(streetResult)(Address.apply)
  }

  println("EMPTY " + checkAddress(Address(29, "")))
  println("CAP   " + checkAddress(Address(29, "acacia road")))

}
