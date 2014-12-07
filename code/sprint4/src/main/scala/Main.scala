/*
In this sprint we "lift" the map, flatMap, and and
operations from Result to Rule. This allows us to combine
rules directly, without wrapping in them in functions.

We now have a pretty good DSL for combining rules in
sequence, although we are still only able to combine
functions in parallel in pairs using `and`.
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

  implicit class RuleOps[A, B](rule: Rule[A, B]) {
    def map[C](func: B => C): Rule[A, C] =
      (a: A) => rule(a) map func

    def flatMap[C](rule2: Rule[B, C]): Rule[A, C] =
      (a: A) => rule(a) flatMap rule2

    def and[C, D](rule2: Rule[A, C])(func: (B, C) => D): Rule[A, D] =
      (a: A) => (rule(a) and rule2(a))(func)
  }

  def rule[A]: Rule[A, A] =
    (input: A) => Pass(input)

  // Application code ---------------------------

  type FormData = Map[String, String]

  case class Address(number: Int, street: String)

  // Validating existing addresses:

  val nonEmpty: Rule[String, String] =
    (str: String) =>
      if(str.isEmpty)
        Fail(List("Empty string"))
      else
        Pass(str)

  val initialCap: Rule[String, String] =
    (str: String) =>
      if(str(0).isUpper)
        Pass(str)
      else
        Fail(List("No initial cap"))

  def capitalize(str: String): String =
    str(0).toUpper +: str.substring(1)

  def gte(min: Int) = (num: Int) =>
    if(num < min) Fail(List("Too small")) else Pass(num)

  val checkNumber: Rule[Address, Int] =
    rule[Address] map (_.number) flatMap gte(1)

  val checkStreet: Rule[Address, String] =
    rule[Address] map (_.street) flatMap nonEmpty map capitalize

  val checkAddress: Rule[Address, Address] =
    (checkNumber and checkStreet)(Address.apply)

  // Reading form data:

  def getField(name: String): Rule[FormData, String] =
    (form: FormData) =>
      form.get(name).
      map(Pass.apply).
      getOrElse(Fail(List(s"Field not found")))

  val parseInt: Rule[String, Int] =
    (str: String) =>
      try {
        Pass(str.toInt)
      } catch {
        case exn: NumberFormatException =>
          Fail(List("Not a number"))
      }

  val readNumber: Rule[FormData, Int] =
    rule[FormData] flatMap getField("number") flatMap parseInt

  val readStreet: Rule[FormData, String] =
    rule[FormData] flatMap getField("street")

  val readAddress: Rule[FormData, Address] =
    (readNumber and readStreet)(Address.apply)

  println("NONUM  " + readAddress(Map("street" -> "acacia Road")))
  println("BADNUM " + readAddress(Map("number" -> "1a", "street" -> "acacia road")))
  println("CAP    " + readAddress(Map("number" -> "29", "street" -> "acacia road")))

}
