# Functional Data Validation

Copyright Dave Gurnell, 2014

Slides licensed under the
<a href="https://creativecommons.org/licenses/by-nc-sa/4.0/">
  Creative Commons BY-NC-SA License, Version 4.0
</a>

Code licensed under the
<a href="http://www.apache.org/licenses/LICENSE-2.0">
  ApacheLicense, Version 2.0
</a>

## Overview

These are the slides and code samples from my talks at Scala Exchange 2014 and ScalaDays Amsterdam 2015. The talk is an entry-level discussion about the thought processes involved in designing library code using functional programming concepts.

The basic thought process outlined is as follows:

 - Define the smallest, simplest, most general building blocks we can
   to model our problem domain. Some building blocks will be data,
   some will be functions.

 - Define ways of *combining* those building blocks to produce larger
   patterns.

The building blocks I define for data validation are:

 - A *validation rule* -- a function.

 - A *validation result* -- an algebraic data type involving success and failure.

We proceed to look at building combinators for these building blocks:

 - combining rules in parallel;
 - combining rules in sequence.

We write simple functional code to implement these combinations, and look at how concepts like *monads* fall naturally from the implementations.

Finally, I look at *lifting* abstractions to different levels to create a simple, declarative DSL for validation.

In the summary I draw comparisons with some of the building blocks available in Scalaz, pointing the listener towards further content.

## Further Reading

Scalaz provides several useful abstractions for data validation. Eugene Yokota covers these in his HTML5 book, *learn you a Scalaz*:

 - [scalaz.Validation](http://eed3si9n.com/learning-scalaz/Validation.html)
 - [Applicative functors and builders](http://eed3si9n.com/learning-scalaz/Applicative.html)

For an in-depth look at four extremely useful Scalaz type classes, check out the training course and forthcoming book that I am co-developing at [Underscore](http://underscore.io):

 - [Essential Scalaz](http://underscore.io/training/courses/essential-scalaz)

The original genesis for this talk came from a code sample I wrote for my ScalaDays 2014 talk on Scala Macros:

 - [Essential Scalaz](http://underscore.io/training/courses/essential-macros)

I have since worked this into a work-in-progress validation library here:

 - [Validation](https://github.com/davegurnell/validation)
