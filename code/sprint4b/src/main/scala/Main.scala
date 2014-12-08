/*
In this sprint (an alternative to sprint4) we deal with arbitrary arity using
"applicative functors".

We refactor our `and` method to the applicative functor method `ap`.
This unfortunately doesn't fall naturally from the problem definition,
so bear with us... instead of:

  trait Result[A] {
    def and[B, C](that: Result[B])(fn: (A, B) => C): Result[C]
  }

we have:

  trait Result[A] {
    def ap[B](fn: Result[A => B]): Result[B]
  }

This is a more general combination method than `and` -- rather than
combine two results with a function, it allows us to convert a single
result using a function.

`ap` looks a little like the `flatMap` method from monad, except that
the types are a little different:

  def flatMap[B](fn: A => Result[B]): Result[B]

  def ap[B](fn: Result[A => B]): Result[B]

Applicative functors are more general than monads. In the case of `Result`:

 - the `fn` parameter to `flatMap` can only yield error messages if
   we call it... we only do this if `this` is a `Pass`, so we end up with the
   familiar short-cutting, error-discarding behaviour we discussed in the talk;

 - the `fn` parameter to `ap` can give us error messages without us having to
   "call" it... we can provide an implementation that preserves
   error messages from `this` and `fn`, which is what we need to replace `and`.

Here's the implementation of `ap` for `Result`... note the similarity with our
previous definition of `and`:

  trait Result[A] {
    def ap[B](fn: Result[A => B]): Result[B] =
      (this, fn) match {
        case (Pass(a), Pass(b)) => Pass(b(a))
        case (Fail(a), Pass(b)) => Fail(a)
        case (Pass(a), Fail(b)) => Fail(b)
        case (Fail(a), Fail(b)) => Fail(a ++ b)
      }
  }

To demonstrate `ap`s usefulness and generality, we can actually implement `and`
in terms of it. To do this we take the `that` and `func` parameters and wrap them
up in a function:

  trait Result[A] {
    def and[B, C](that: Result[B])(func: (A, B) => C): Result[C] =
      ap(that.map((b: B) => (a: A) => func(a, b)))

    // ...
  }

Take the time to examine this complex bit of type gymnastics and work out what
is going on. The essence of the transformation is that we are converting our
binary function:

  (A, B) => C

into a *curried* form:

  B => A => C

We can implement a three-Result version of `and` by nesting two calls to `ap`.

In fact, if we want to run `N` different rules on an input and pass the results
into an `N`-argument constructor function, here's what we do:

  1. Convert the constructor to curried form:

       (a: A, b: B, c: C, ...) => R

     becomes:

       a: A => b: B => c: C => ... => R

  2. Wrap the constructor in a `Pass`:

       Pass(a: A => b: B => c: C => ... => R)

  3. Call the first field-checking rule, call `ap`, and pass the `Pass`
     to `ap` as a parameter:

       checkA(form) ap {
         Pass(a: A => b: B => c: C => ... => R)
       }

  4. The result of this expression is of type:

       Result[B => C => ... => R]

     in other words, we've chopped the first argument off of our function call.
     Now we can pass this result to the `ap` method of the rule for our second
     argument:

       checkB(form) ap {
         checkA(form) ap {
           Pass(a: A => b: B => c: C => ... => R)
         }
       }

  5. Rinse and repeat until we've run out of arguments.

This all takes some explaining, and it's all pretty verbose. I think the reason
applicative functors are so well known is that they come from Haskell. In Haskell,
all functions are curried by default, so the syntax for this kind of thing is
beautifully short and elegant. In Scala it's a bit of a burden.

In fact, this stuff is so burdensome, libraries like Scalaz provide an alternate
syntax called *applicative builder syntax* that does a similar thing. The idea is
as follows:

  1. Combine two results together using a method, let's call it `|@|`
     (because that's what Scalaz calls it), to produce an object of type `Builder2`:

     (a |@| b)

  2. `Builder2` has an `apply` method that accepts a function with two arguments
     and returns a `Result` of the result type:

     val result: Result[C] = (a |@| b)((a: A, b: B) => new C(a, b))

  3. `Builder2` also has an `|@|` method that creates a `Builder3` with an `apply`
     method that accepts three arguments:

     val result: Result[D] = (a |@| b |@| c)((a: A, b: B, c: C) => new D(a, b, c))

  4. And so on up to Builder22...

So the applicative builder pattern provides a much nicer syntax for doing applicative
functor-like things, although it can only scale to a certain arity (because your library
code needs `Builder2` and onwards up to `BuilderMaxArity`).

Applicative functors on their own can be recursively nested up to arbitrary arity,
and the nested tuple case in sprint4 can also work to arbitrary arity.

NOTE: I haven't attempted to lift `ap` from `Result` to `Rule` as I did with
`and` in sprint 3. I honestly haven't thought about whether this would be easy
or difficult, or how the code would look once it had been lifted. I'll leave this
as an exercise to the reader :)

*/
object Main extends App {

  // Validation library -------------------------

  sealed trait Result[+A] {
    def ap[B](fn: Result[A => B]): Result[B] =
      (this, fn) match {
        case (Pass(a), Pass(b)) => Pass(b(a))
        case (Fail(a), Pass(b)) => Fail(a)
        case (Pass(a), Fail(b)) => Fail(b)
        case (Fail(a), Fail(b)) => Fail(a ++ b)
      }

    def and[B, C](that: Result[B])(func: (A, B) => C): Result[C] =
      ap(that.map((b: B) => (a: A) => func(a, b)))

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

  case class Address(number: Int, street: String, zipCode: String)

  // Validating existing addresses:

  val nonEmpty: Rule[String, String] =
    (str: String) =>
      if(str.isEmpty) Fail(List("Empty string")) else Pass(str)

  val initialCap: Rule[String, String] =
    (str: String) =>
      if(str(0).isUpper) Pass(str) else Fail(List("No initial cap"))

  def capitalize(str: String): String =
    str(0).toUpper +: str.substring(1)

  def gte(min: Int) = (num: Int) =>
    if(num < min) Fail(List("Too small")) else Pass(num)

  val checkNumber: Rule[Address, Int] =
    rule[Address] map (_.number) flatMap gte(1)

  val checkStreet: Rule[Address, String] =
    rule[Address] map (_.street) flatMap nonEmpty map capitalize

  val checkZip: Rule[Address, String] =
    rule[Address] map (_.zipCode) flatMap nonEmpty

  val checkAddress: Rule[Address, Address] =
    (address: Address) =>
      checkZip(address) ap {
        checkStreet(address) ap {
          checkNumber(address) ap {
            Pass {
              (number: Int) =>
              (street: String) =>
              (zipCode: String) =>
              Address(number, street, zipCode)
            }
          }
        }
      }

  // Reading form data:

  def getField(name: String): Rule[FormData, String] =
    (form: FormData) =>
      form.get(name) map (Pass.apply) getOrElse Fail(List(s"Field not found"))

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

  val readZip: Rule[FormData, String] =
    rule[FormData] flatMap getField("zip")

  val readAddress: Rule[FormData, Address] =
    (form: FormData) =>
      readZip(form) ap {
        readStreet(form) ap {
          readNumber(form) ap {
            Pass {
              (number: Int) =>
              (street: String) =>
              (zipCode: String) =>
              Address(number, street, zipCode)
            }
          }
        }
      } flatMap checkAddress

  println("GOOD " + readAddress(Map("number" -> "29", "street" -> "acacia road", "zip" -> "ABC 123")))
  println("BAD  " + readAddress(Map("number" -> "-1", "street" -> "", "zip" -> "")))

}
