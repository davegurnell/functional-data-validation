/*
In this sprint we validate incoming form data as well as
existing Addresses.

We give Rules the ability to perform arbitrary type
conversions on the data being validated.
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

  type Rule[-A, +B] = A => Result[B]

  // Application code ---------------------------

  type FormData = Map[String, String]

  case class Address(number: Int, street: String)

  // Validating existing addresses:

  def nonEmpty(str: String): Result[String] =
    if(str.isEmpty) Fail(List("Empty string")) else Pass(str)

  def initialCap(str: String): Result[String] =
    if(str(0).isUpper) Pass(str) else Fail(List("No initial cap"))

  def capitalize(str: String): String =
    str(0).toUpper +: str.substring(1)

  def gte(min: Int)(num: Int) =
    if(num < min) Fail(List("Too small")) else Pass(num)

  def checkStreet(str: String): Result[String] =
    nonEmpty(str).flatMap(initialCap)
    // or nonEmpty(str).map(capitalize)

  def checkAddress(addr: Address) = {
    val numberResult = gte(1)(addr.number)
    val streetResult = checkStreet(addr.street)

    numberResult.and(streetResult)(Address.apply)
  }

  // Reading form data:

  def getField(name: String): Rule[FormData, String] =
    (form: FormData) =>
      form.get(name).
      map(Pass.apply).
      getOrElse(Fail(List(s"Field not found")))

  def parseInt(str: String): Result[Int] =
    try {
      Pass(str.toInt)
    } catch {
      case exn: NumberFormatException =>
        Fail(List("Not a number"))
    }

  def readAddress(form: FormData): Result[Address] = {
    val numberResult = getField("number")(form).flatMap(parseInt)
    val streetResult = getField("street")(form)

    numberResult.and(streetResult)(Address.apply).flatMap(checkAddress)
  }

  println("NONUM  " + readAddress(Map("street" -> "acacia Road")))
  println("BADNUM " + readAddress(Map("number" -> "1a", "street" -> "acacia road")))
  println("CAP    " + readAddress(Map("number" -> "29", "street" -> "acacia road")))

}
