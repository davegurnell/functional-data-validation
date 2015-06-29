/*
In this sprint we set up a basic validation library:

 - A rule is a function from a value to a result
 - A result is a pass or a fail
 - A fail contains a list of error messages

We can combine results with `and` (append semantics),
and combine rules by running them and combining their
results.
*/
object Main extends App {

  // Validation library -------------------------

  sealed trait Result {
    def and(that: Result) = (this, that) match {
      case (Pass,    Pass   ) => Pass
      case (Fail(a), Pass   ) => Fail(a)
      case (Pass,    Fail(b)) => Fail(b)
      case (Fail(a), Fail(b)) => Fail(a ++ b)
    }
  }

  final case object Pass extends Result
  final case class Fail(messages: List[String]) extends Result

  type Rule[A] = A => Result

  // Application code ---------------------------

  case class Address(number: Int, street: String)

  val nonEmpty: Rule[String] =
    (str: String) =>
      if(str.isEmpty) Fail(List("Empty string")) else Pass

  def gte(min: Int): Rule[Int] =
    (num: Int) =>
      if(num < min) Fail(List("Too small")) else Pass

  val checkAddress: Rule[Address] =
    (addr: Address) =>
      gte(1)(addr.number) and nonEmpty(addr.street)

  println("PASS " + checkAddress(Address(29, "Acacia Road")))
  println("FAIL " + checkAddress(Address(0, "")))

}
