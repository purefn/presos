---
title: Free your monads
author: Richard Wallace
date: 2017-01-09
patat:
    wrap: true
    incrementalLists: true
    theme:
        header: [vividGreen, bold]
        code: [vividYellow]
...

     ___              __  __                  _
    | __| _ ___ ___  |  \/  |___ _ _  __ _ __| |___
    | _| '_/ -_) -_) | |\/| / _ \ ' \/ _` / _` (_-<
    |_||_| \___\___| |_|  |_\___/_||_\__,_\__,_/__/

                Step by step

# Obligatory Monad refresher

```scala
trait Monad[M[_]] {
  def point[A](a: => A): M[A]
  def bind[A, B](fa: M[A])(f: A => M[B]): M[B]
}
```

# Obligatory Monad refresher

* Monads are a triple
    * M[_] is a type constructor
    * `point` lifts a pure value into the computation context
    * `bind` allows you to use a value from a previous computation in a new computation
* in FP, often used to control *effects*

# Monads for effect - Reader

> * `Reader[R, A]` provides read access to some environment value of type `R`
```scala
val greeting: Reader[String, String] =
  for {
    name <- ask[String]
  } yield s"Hello, $name!"

println(greeting.run("everyone!"))
```

# Monads for effect - State

> * `State[S, A]` provides read and write access to computation state of type `S`
```scala
val nextRand: State[PsuedoRng, Double] =
  for {
    rng         <- get[Psuedorng]
    (newRng, a) = rng.next
    _           <- put(newRng)
  } yield a

val nextRandBool: State[PsuedoRng, Boolean] =
  nextRand.map(_ > 0.5)
```

# Monads for effect - State

> * `State[S, A]` provides read and write access to computation state of type `S`
```scala
val threeCoins: State[PsuedoRng, (Boolean, Boolean, Boolean)] =
  for {
    a <- nextRandBool
    b <- nextRandBool
    c <- nextRandBool
  } yield (a, b, c)

threeCoins.eval(PsuedoRng(seed))
```
* But what if we decide we don't want to use a psuedo-random generator?

# Monads for effect - IO

> * `IO[A]` provides the ability to have side-effects and be referentially transparent
```scala
val prompt =
  for {
    _    <- IO.putStrLn("Who are you?")
    name <- IO.readLn
    _    <- IO.putStrLn(s"I'm $name!")
  } yield ()

prompt.unsafePerformIO // side-effects occur here!
```
* But how can we test that `prompt: IO[Unit]` is correct?

# Monads for effect - issues so far

* How can we alter the way our program runs without rewriting it?
* How can we test that our program does what we expect?
* What if we could write *one* program, and interpret it in different ways?
    * One interpreter for testing
    * One interpreter for production
    * One interpreter for metrics

# DSLs

> * Build a one domain specific language, write many interpreters
```scala
sealed trait Rng[A]
final case class RngGen[A](next: Double => A) extends Rng[A]
```
* Build an interpreter that uses a psuedo-random generator
* Build an interpreter that reads entropy from a device
* We can write our program and decide later which to use

# DSLs

> * Build a one domain specific language, write many interpreters
```scala
sealed trait Console[A]
final case class PutStr(str: String, next: A) extends Console[Unit]
final case class ReadLn[A](next: String => A): extends Console[A]
```
* Build an interpreter that collects values from `PutStr` in a list and supplies values to `ReadLn` from another list
* Build an interpreter that prints values from `PutStr` to `stdout` and reads values from `stdin` for `ReadLn`

# DSLs

* GREAT WORK! WE'VE SOLVED THE PROBLEM!
* Except... how do we use them?

# DSLs

> * `Rng` is not a monad

```scala
for {
  a <- RngGen(identity)
  b <- RngGen(identity)
} yield (a, b)
```

# DSLs

> * `Console` isn't a monad either

```scala
for {
  _ <- PutStr("What's your name?", ())
  n <- ReadLn(identity)
} yield n
```

# DSLs

> * GREAT WORK! WE'VE SOLVED THE PROBLEM!
> * Except... how do we combine them?

* We *could* write a bunch of monad instances, painful if our language is large
* Can we somehow get those for (drevil) *free*?

# What makes a monad?

* A monad is a triple
    * some type constructor `F`
    * `point: A => F[A]`
    * `bind: F[A] => (A => F[B]) => F[B])`

# What makes a monad?

* Can we encode that directly?
* Let's revisit our goal...
    * Make monads for things like `Rng` and `Console`
    * `Rng` and `Console` are both type constructors, so maybe...

# A monad has a type constructor

* We create type constructor that abstracts over another type constructor
```scala
sealed trait FM[F[_], A]
```

# A monad has a `point` function

* We create a value constructor for lifting a value
```scala
sealed trait FM[F[_], A]
final case class Point[F[_], A](a: A) extends FM[F, A]
```

# A monad has a `bind` function

* What about for getting the next expression to run?
* What are we trying to represent?
    * A series of expressions in our DSL, the next depending on the previous

# A monad has a `bind` function

* Given `prg: FM[Rng, A]`, what would evaluating a single step look like?
* `prg = Point(a)`
    * `step(prg) = a`
    * the final result of running our interpreter

# A monad has a `bind` function

* otherwise, we're expecting a new expression
    * `step(prg) = RngGen(f)` where `f: Double => FM[Rng, A]`
    * a step for our interpreter to process, which yields the remainder of the computation
    * `RngGen(f): Rng[FM[Rng, A]]`

# A monad has a `bind` function

> * Making `Rng[FM[Rng, A]]` generic gives us `F[FM[F, A]]`

```scala
sealed trait FM[F[_], A]
final case class Point[F[_], A](a: A) extends FM[F, A]
final case class Impure[F[_], A](a: F[FM[F, A]]) extends FM[F, A]
```

# Now we can build a monad instance for FM

> * We partially apply the type, given some type constructor `F`

```scala
abstract class FMMonad[F[_]] extends Monad[FM[F, ?]] {
}
```

# Now we can build a monad instance for FM

> * `point` is trivial

```scala
abstract class FMMonad[F[_]] extends Monad[FM[F, A]] {
  def point[A](a: => A): FM[F, A] = Point(a)
}
```

# Now we can build a monad instance for FM

> * `bind` requires a bit more work

```scala
abstract class FMMonad[F[_]] extends Monad[FM[F, A]] {
  def point[A](a: => A): FM[F, A] = Point(a)
  def bind[A, B](fa: FM[F, A])(f: A => FM[F, B]): FM[F, B] =
    ???
}
```

# Now we can build a monad instance for FM

> * Need to inspect `fa`

```scala
abstract class FMMonad[F[_]] extends Monad[FM[F, A]] {
  def point[A](a: => A): FM[F, A] = Point(a)
  def bind[A, B](fa: FM[F, A])(f: A => FM[F, B]): FM[F, B] =
    fa match {
      case Point(a) => ???
      case Impure(a) => ???
    }
}
```

# Now we can build a monad instance for FM

> * The `Point` case is trivial, just apply the function

```scala
abstract class FMMonad[F[_]] extends Monad[FM[F, A]] {
  def point[A](a: => A): FM[F, A] = Point(a)
  def bind[A, B](fa: FM[F, A])(f: A => FM[F, B]): FM[F, B] =
    fa match {
      case Point(a) => f(a)
      case Impure(a) => ???
    }
}
```

# Now we can build a monad instance for FM

> * In the `Impure` case, `a: F[FM[F, A]` and we need an `FM[F, B]`

```scala
abstract class FMMonad[F[_]] extends Monad[FM[F, A]] {
  def point[A](a: => A): FM[F, A] = Point(a)
  def bind[A, B](fa: FM[F, A])(f: A => FM[F, B]): FM[F, B] =
    fa match {
      case Point(a) => f(a)
      case Impure(a) => ???
    }
}
```

# Now we can build a monad instance for FM

> * We have an `F[FM[F, A]]` and a `A => FM[F, B]`, hmmm....

```scala
abstract class FMMonad[F[_]] extends Monad[FM[F, A]] {
  def point[A](a: => A): FM[F, A] = Point(a)
  def bind[A, B](fa: FM[F, A])(f: A => FM[F, B]): FM[F, B] =
    fa match {
      case Point(a) => f(a)
      case Impure(a) => ???
    }
}
```

# Now we can build a monad instance for FM

> * Recursion to the rescue!

```scala
abstract class FMMonad[F[_]] extends Monad[FM[F, A]] {
  def point[A](a: => A): FM[F, A] = Point(a)
  def bind[A, B](fa: FM[F, A])(f: A => FM[F, B]): FM[F, B] =
    fa match {
      case Point(a) => f(a)
      case Impure(a) => Impure(Functor[F].map(a)(bind(_)(f)))
    }

  implicit def F: Functor[F]
}
```

# Now we can build a monad instance for FM

* Lets break `Impure(Functor[F].map(a)(bind(_)(f)))` down a bit
* We have a `F[FM[F, A]`, so we need to use `map`, which means we need a `Functor[F]`
* Inside the `map`, we have a `FM[F, A]` which we can recursively call `bind` on
* This gives us a `F[FM[F, B]`, so we package it up in an `Impure` value constructor
* At the end, we have a `FM[F, B]`

# We have a free monad...

* for any functor, `F`
* `s/FM/Free`
```scala
sealed trait Free[F[_], A]
final case class Point[F[_], A](a: A) extends Free[F, A]
final case class Impure[F[_], A](a: F[Free[F, A]]) extends Free[F, A]
```

# Back to our Rng

* We'll need a `Functor[Rng]`

```scala
sealed trait Rng[A]
final case class RngGen[A](next: Double => A) extends Rng[A]
```

# Back to our Rng

> * We'll need a `Functor[Rng]`

```scala
sealed trait Rng[A]
final case class RngGen[A](next: Double => A) extends Rng[A]

class RngFunctor extends Functor[Rng] {
}
```

# Back to our Rng

> * We'll need a `Functor[Rng]`

```scala
sealed trait Rng[A]
final case class RngGen[A](next: Double => A) extends Rng[A]

class RngFunctor extends Functor[Rng] {
  def map[A, B](fa: Rng[A])(f: A => B): Rng[B] =
    ???
}
```

# Back to our Rng

> * We'll need a `Functor[Rng]`

```scala
sealed trait Rng[A]
final case class RngGen[A](next: Double => A) extends Rng[A]

class RngFunctor extends Functor[Rng] {
  def map[A, B](fa: Rng[A])(f: A => B): Rng[B] =
    Rng(fa.next andThen f)
}
```

# Back to our Rng

> * How do we use it?

```scala
for {
  a <- RngGen(x => x)
  b <- RngGen(x => x)
} yield (a, b)
```

# Back to our Rng

> * We need to lift into Free

```scala
val nextRand: Free[Rng, Double] =
  ???
```

# Back to our Rng

> * `Point` is not going to do it, we need `Impure`

```scala
val nextRand: Free[Rng, Double] =
  Impure(???)
```

# Back to our Rng

> * Which means we need a `Rng[Free[Rng, Double]]`

```scala
val nextRand: Free[Rng, Double] =
  Impure(RngGen(???))
```

# Back to our Rng

> * `RngGen` needs a `Double => A`

```scala
val nextRand: Free[Rng, Double] =
  Impure(RngGen(x => ???))
```

# Back to our Rng

> * The `A` in `RngGen[A]` needs to be a `Free[Rng, Double]` because we are looking for `Rng[Free[Rng, A]]`

```scala
val nextRand: Free[Rng, Double] =
  Impure(RngGen(x => Point(x)))
```

# Back to our Rng

> * Now we can use it!

```scala
for {
  a <- nextRand
  b <- nextRand
} yield (a, b)
```

* This expression has type `Free[Rng, (Double, Double)]`

# Our first combinator

* We can generalize `nextRand`

# Our first combinator

> * Given some `F[A]` where `F` is a functor, we want to lift that into `Free`

```scala
def liftF[F[_]: Functor, A](fa: F[A]): Free[F, A] =
  ???
```

# Our first combinator

> * Just like before, `Impure` is the only thing that will do

```scala
def liftF[F[_]: Functor, A](fa: F[A]): Free[F, A] =
  Impure(???)
```

# Our first combinator

> * Need to go from `F[A]` to `F[Free[F, A]]`, looks like `map`

```scala
def liftF[F[_]: Functor, A](fa: F[A]): Free[F, A] =
  Impure(Functor[F].map(fa)(???))
```

# Our first combinator

> * Now we have an `A` and need a `Free[F, A]`, which is `Point`

```scala
def liftF[F[_]: Functor, A](fa: F[A]): Free[F, A] =
  Impure(Functor[F].map(fa)(Point(_)))
```

# Our first combinator

> * We can rewrite `nextRand` to use it

```scala
def liftF[F[_]: Functor, A](fa: F[A]): Free[F, A] =
  Impure(Functor[F].map(fa)(Point(_)))

val nextRand: Free[Rng, Double] = liftF(RngGen(identity))
```

# Our first combinator

> * And we can use it for `putStr` and `readLn` too

```scala
def liftF[F[_]: Functor, A](fa: F[A]): Free[F, A] =
  Impure(Functor[F].map(fa)(Point(_)))

val nextRand: Free[Rng, Double] = liftF(RngGen(identity))

def putStr(x: String): Free[Console, Unit] = liftF(PutStr(x))
val readLn: Free[Console, String] = liftF(ReadLn(identity))
```

# We have a DSL!

* GREAT WORK! WE'VE SOLVED THE PROBLEM!
* Except... how do we combine them?

# We have a DSL!

* `Free[Rng, Double]` cannot be combined with `Free[Console, Unit]`

```scala
for {
  a <- nextRand
  _ <- putStr(s"$a")
} yield ()
```

# Suppose we could combine them

* assume we have some type to combine `F` and `G` into `FG`
* What would a single step look like?
    * `step(prg) => F[Free[FG, A]]`
    * `step(prg) => G[Free[FG, A]]`
    * Stepping gives either an `F` or `G` for our interpreter

# Suppose we could combine them

* This sounds familiar...
* Maybe `F[A] \/ G[A]` ?
* Doesn't quite fit what `Free` needs, an `F[_]`
* `type CP[F[_], G[_], A] = F[A] \/ G[A]` fits!

# Combining DSLs

* We call these `Coproduct`s

```scala
final case class Coproduct[F[_], G[_], A](e: F[A] \/ G[A])
```

# Combining DSLs

> * But are they `Functor`s?

# Combining DSLs

> * If `F` and `G` are, yes!

```scala
class CoproductFunctor[F[_], G[_]] extends Functor[Coproduct[F, G, ?]] {
  implicit def F: Functor[F]
  implicit def G: Functor[G]
}
```

# Combining DSLs

> * We extract the disjunction and destructure it into the two cases

```scala
class CoproductFunctor[F[_], G[_]] extends Functor[Coproduct[F, G, ?]] {
  implicit def F: Functor[F]
  implicit def G: Functor[G]

  def map[A, B](a: Coproduct[F, G, A])(f: A => B): Coproduct[F, G, B] =
    a.e match {
      case Left(fa) => ???
      case Right(ga) => ???
    }
}
```
# Combining DSLs

> * If it's `Left`, we use the functor for `F`

```scala
class CoproductFunctor[F[_], G[_]] extends Functor[Coproduct[F, G, ?]] {
  implicit def F: Functor[F]
  implicit def G: Functor[G]

  def map[A, B](a: Coproduct[F, G, A])(f: A => B): Coproduct[F, G, B] =
    a.e match {
      case Left(fa) => Coproduct(Left(Functor[F].map(fa)(f)))
      case Right(ga) => ???
    }
}
```

# Combining DSLs

> * If it's `Right`, we use the functor for `G`

```scala
class CoproductFunctor[F[_], G[_]] extends Functor[Coproduct[F, G, ?]] {
  implicit def F: Functor[F]
  implicit def G: Functor[G]

  def map[A, B](a: Coproduct[F, G, A])(f: A => B): Coproduct[F, G, B] =
    a.e match {
      case Left(fa) => Coproduct(Left(Functor[F].map(fa)(f)))
      case Right(ga) => Coproduct(Right(Functor[G].map(ga)(f)))
    }
}
```

# Combining DSLs

* We can build arbitrarily large DSLs
* `Coproduct[Rng, Console, A]` is a functor
* so is `Coproduct[Http, Coproduct[Rng, Console, ?], A]`
* and so on

# Combining DSLs

> * Now we can write

```scala
for {
  a <- liftF(Coproduct(Left(RngGen(identity))))
  _ <- liftF(Coproduct(Right(PutStr(s"$a", ()))))
} yield ()
```

# Combining DSLs

* But what a mess! Let's generalize!
* We want to go from `F[A]` => `Free[Coproduct[F, G, ?], A]`
    * `def liftLeftF[F[_], G[_], A](fa: F[A]): Free[Coproduct[F, G, ?] A]`
* or from `G[A]` => `Free[Coproduct[F, G, ?], A]`
    * `def liftRightF[F[_], G[_], A](ga: G[A]): Free[Coproduct[F, G, ?] A]`

# Combining DSLs

> * Still not ideal

```scala
for {
  a <- liftLeftF(RngGen(identity)))
  _ <- liftRightF(PutStr(s"$a", ()))
} yield ()
```

# Combining DSLs

> * We really want to be able to write

```scala
for {
  a <- nextRand
  _ <- putStr(s"$a")
} yield ()
```

# Combining DSLs

> * What do we need to accomplish that?

```scala
def nextRand: Free[Coproduct[Rng, Console, ?], Double]

def putStr(x: String): Free[Coproduct[Rng, Console, ?], Unit]
```

# Combining DSLs

> * Generalize each one

```scala
def nextRand[F[_]]: Free[Coproduct[Rng, F, ?], Double]

def putStr[F[_]](x: String): Free[Coproduct[F, Console, ?], Unit]
```

# Combining DSLs

> * But what if `Rng` is on the left side and `Console` on the right?

```scala
def nextRand[F[_]]: Free[Coproduct[F, Rng, ?], Double]

def putStr[F[_]](x: String): Free[Coproduct[Console, F, ?], Unit]
```

# Combining DSLs

> * `nextRand` and `putStr` shouldn't have to care about order!

```scala
def nextRand[F[_]]: Free[F, Double]

def putStr[F[_]](x: String): Free[F, Unit]
```

# Combining DSLs

> * We need something which relates `Rng` to `F`

```scala
trait I[F[_], G[_]]
```

# Combining DSLs

> * It should give us some way of going from `Rng[A] => F[A]`

```scala
trait I[F[_], G[_]] {
  def i[A](fa: F[A]): G[A]
}
```

# Combining DSLs

> * Commonly known as 'injection' (no, not that kind!)

```scala
trait Inject[F[_], G[_]] {
  def inj[A](fa: F[A]): G[A]
}
```

# Combining DSLs

> * We can write the reflexive instance, `F[A] => F[A]`

```scala
class ReflexInject[F[_]] extends Inject[F, F] {
  def inj[A](fa: F[A]): F[A] = fa
}
```

# Combining DSLs

> * We can also write instance for `F` on the left of a coproduct

```scala
class LeftInject[F[_], G[_]] extends Inject[F, Coproduct[F, G, ?]] {
  def inj[A](fa: F[A]): Coproduct[F, G, A] =
    Coproduct(Left(fa))
```

# Combining DSLs

* And for `F` on the right of a coproduct
* But remember, we'd like to build things like `Coproduct[Http, Coproduct[Rng, Console, ?], A]`
* If we define right injection as we did for left, we couldn't do it in one shot

# Combining DSLs

> * We need to recursively embed `F` on the right

```scala
class RightInject[F[_], G[_], H[_]] extends Inject[F, Coproduct[H, G, ?]]
```

# Combining DSLs

> * To do that, `F` needs to be injectable to `G`

```scala
class RightInject[F[_], G[_], H[_]] extends Inject[F, Coproduct[H, G, ?]] {
  def FG: Inject[F, G]

  def inj[A](fa: F[A]): Coproduct[H, G, ?] =
    ???
}
```

# Combining DSLs

> * Embed to the right

```scala
class RightInject[F[_], G[_], H[_]] extends Inject[F, Coproduct[H, G, ?]] {
  def FG: Inject[F, G]

  def inj[A](fa: F[A]): Coproduct[H, G, ?] =
    Coproduct(Right(???))
}
```

# Combining DSLs

> * We have a `F[A]`, We need a `G[A]`, so we inject it

```scala
class RightInject[F[_], G[_], H[_]] extends Inject[F, Coproduct[H, G, ?]] {
  def FG: Inject[F, G]

  def inj[A](fa: F[A]): Coproduct[H, G, ?] =
    Coproduct(Right(FG.inj(fa)))
}
```

# Combining DSLs

> * Revisiting `nextRand` and `putStr`, we ended with

```scala
def nextRand[F[_]]: Free[F, Double] =
  ???

def putStr[F[_]](x: String): Free[F, Unit] =
  ???
```

# Combining DSLs

> * Now we can relate them to `F`

```scala
def nextRand[F[_]](implicit i: Inject[Rng, F]): Free[F, Double] =
  ???

def putStr[F[_]](x: String)(implicit i: Inject[Console, F]): Free[F, Unit] =
  ???
```

# Combining DSLs

> * We start with our basic value constructors

```scala
def nextRand[F[_]](implicit i: Inject[Rng, F]): Free[F, Double] =
  ???(RndGen(identity))

def putStr[F[_]](x: String)(implicit i: Inject[Console, F]): Free[F, Unit] =
  ???(PutStr(x, ()))
```
# Combining DSLs

> * Then we inject them into `F`

```scala
def nextRand[F[_]](implicit i: Inject[Rng, F]): Free[F, Double] =
  ???(i.inj(RndGen(identity)))

def putStr[F[_]](x: String)(implicit i: Inject[Console, F]): Free[F, Unit] =
  ???(i.inj(PutStr(x, ())))
```

# Combining DSLs

> * Finally, we lift them into `Free`, which requires a functor

```scala
def nextRand[F[_]: Functor](implicit i: Inject[Rng, F]): Free[F, Double] =
  liftF(i.inj(RndGen(identity)))

def putStr[F[_]: Functor](x: String)(implicit i: Inject[Console, F]): Free[F, Unit] =
  liftF(i.inj(PutStr(x, ())))
```

# Combining DSLs

> * Now we can write

```scala
for {
  a <- nextRand
  _ <- putStr(s"$a")
} yield ()
```

# Combining DSLs

* Haha! Just kidding!
* Scala can't infer the proper types
* At best, it will tell you so
* At worst, it will crash with a SOE!

# Combining DSLs

> * Always be Annotating

```scala
for {
  a <- nextRand[Coproduct[Rng, Console, ?]]
  _ <- putStr[Coproduct[Rng, Console, ?]](s"$a")
} yield ()
```

# Combining DSLs

* We can use a type alias to clean it up a bit
* `type App[A] = Coproduct[Rng, Console, ?]`
* But we'll still have to annotate everything

# Some nicer APIs

> * Let's abstract `nextRand` and other functions

# Some nicer APIs

> * First, let's restructure a bit

```scala
object Rng {
  sealed trait Op[A]
  final case class Gen[A](gen: Double => A) extends Op[A]
}
```

# Some nicer APIs

> * Define an abstraction over a type constructor

```scala
trait Rng[F[_]]
```

# Some nicer APIs

> * Define a function for each of our value constructors

```scala
trait Rng[F[_]] {
  def gen: F[Double]
}
```

# Some nicer APIs

> * Define an instance for `Free[F, ?]`

```scala
abstract class FreeRng[F[_]] extends Rng[Free[F, ?]]
```

# Some nicer APIs

> * Need a way to relate `Rng.Op` to `F`

```scala
abstract class FreeRng[F[_]] extends Rng[Free[F, ?]] {
  def I: Inject[Rng.Op, F]
}
```

# Some nicer APIs

> * Our implementation is the same as `nextRand`

```scala
abstract class FreeRng[F[_]] extends Rng[Free[F, ?]] {
  def I: Inject[Rng.Op, F]
  def F: Functor[F]

  val gen: Free[F, Double] =
    liftF(I.inj(Rng.Gen(identity)))
}
```

# Some nicer APIs

> * We can do the same for `Console`

```scala
abstract class FreeConsole[F[_]] extends Console[Free[F, ?]] {
  def I: Inject[Console.Op, F]
  def F: Functor[F]

  val readLn: Free[F, String] =
    liftF(I.inj(Console.ReadLn(identity)))

  def putStr(x: String): Free[F, Unit] =
    liftF(I.inj(Console.PutStr(x, ())))
}
```

# Some nicer APIs

> * Now our previous program can become

```scala
def prg[F[_]: Monad)(implicit console: Console[F], rng: Rng[F]): F[Unit] =
  for {
    a <- rng.gen
    _ <- console.putStr(s"$a")
  } yield ()
```

# Some nicer APIs

> * And build it with `Free`

```scala
type AppOp[A] = Coproduct[Rng, Console, ?]
type App[A] = Free[AppOp, A]
prg[App]
```

# YAY WE'VE REALLY DONE IT THIS TIME!

* We can get monads for free for our DSLs
* We can combine small DSLs to get big DSLs
* We have nice APIs we can use so we don't have to annotate (allthethings)
* But wait... how do we actually run these things?

# Interpreters

* In computation, interpreters take a program and "do something" with it
* Execute it
    * Given `prg: F[A]`, produce an `IO[A]`
* Rewrite it in another form (optimization, compile to another language, etc.)
    * Given `prg: F[A]`, produce some `G[A]`

# Interpreters

> * Interpreters are just natural transformations

```scala
trait ~>[F[_], G[_] { self =>
  def apply[A](fa: F[A]): G[A]
}
```

# Interpreters

* Recall our theoretical `step` function?
* It yields the next computation for our interpreter, or the end value

# Interpreters

> * If we apply `step` iteratively...

```scala
step(prg) : F[Free[F, A]]
```

# Interpreters

> * If we apply `step` iteratively...

```scala
step(prg) : F[Free[F, A]]
step(prg).map(step) : F[F[Free[F, A]]]
```

# Interpreters

> * We keep going until we get an `A` from `step`

```scala
step(prg) : F[Free[F, A]]
step(prg).map(step) : F[F[Free[F, A]]]
step(prg).map(step).map(_.map(step)) : F[F[F[Free[F, A]]]]
```

# Interpreters

> * And apply an interpreter at each step

```scala
val interp: F ~> G
val stepi = step andThen interp.apply
```

# Interpreters

> * And apply an interpreter at each step

```scala
val interp: F ~> G
val stepi = step andThen interp.apply
stepi(prg) : G[Free[F, A]]
```

# Interpreters

> * And apply an interpreter at each step

```scala
val interp: F ~> G
val stepi = step andThen interp.apply
stepi(prg) : G[Free[F, A]]
stepi(prg).map(stepi) : G[G[Free[F, A]]]
```

# Interpreters

> * And apply an interpreter at each step

```scala
val interp: F ~> G
val stepi = step andThen interp.apply
stepi(prg) : G[Free[F, A]]
stepi(prg).map(stepi) : G[G[Free[F, A]]]
stepi(prg).map(stepi).map(_.map(stepi)) : G[G[G[Free[F, A]]]]
```

# Interpreters

> * If `G` is a monad, we can `flatMap` each step

```scala
val interp: F ~> G
val stepi = step andThen interp.apply
stepi(prg) : G[Free[F, A]]
stepi(prg).flatMap(stepi) : G[Free[F, A]]
stepi(prg).flatMap(stepi).flatMap(stepi) : G[Free[F, A]]
```

# Interpreters

* We can write a function to do that!
* But first, let's give `step` a proper type
    * It yields *either* a value or the next computation

```scala
def step[F[_], A](fa: Free[F, A]): F[Free[F, A]] \/ A
```

# Interpreters

> * Now we can write our function!

```scala
def foldMap[F[_]: Functor, G[_]: Monad, A](fa: Free[F, A])(f: F ~> M): M[A] =
  ???
```

# Interpreters

> * Apply `step` and handle each case

```scala
def foldMap[F[_]: Functor, G[_]: Monad, A](fa: Free[F, A])(f: F ~> M): M[A] =
  step(fa) match {
    case Left(a) => ???
    case Right(a) => ???
  }
```

# Interpreters

> * For `Right`, it's just a value, so...

```scala
def foldMap[F[_]: Functor, G[_]: Monad, A](fa: Free[F, A])(f: F ~> M): M[A] =
  step(fa) match {
    case Left(a) => ???
    case Right(a) => Monad[G].point(a)
  }
```

# Interpreters

> * For `Left`, we have `F[Free[F, A]]`, so we'll apply `f`

```scala
def foldMap[F[_]: Functor, G[_]: Monad, A](fa: Free[F, A])(f: F ~> M): M[A] =
  step(fa) match {
    case Left(a) => ???(f(a))
    case Right(a) => Monad[G].point(a)
  }
```

# Interpreters

> * Now we have a `G[Free[F, A]]`, time to recurse!

```scala
def foldMap[F[_]: Functor, G[_]: Monad, A](fa: Free[F, A])(f: F ~> G): G[A] =
  step(fa) match {
    case Left(a) => Monad[G].bind(f(a))(foldMap(_)(f))
    case Right(a) => Monad[G].point(a)
  }
```

# Interpreters

* Now, given `prg: Free[Rng, A]` and `f: Rng ~> Id`, we can execute it
* `foldMap(prg)(f)`
* But what about `prg: Free[Coproduct[Rng, Console, ?], A]`?
* We can write an interpreter for that too!

# Interpreters

> * We want an interpreter with type `Coproduct[Rng, Console, A] ~> G[A]`

```scala
val interp = new (Coproduct[Rng, Console, ?] ~> G) {
  def apply[A](a: Coproduct[Rng, Console, A]): G[A] =
    ???
}
```

# Interpreters

> * Inspect the coproduct

```scala
val interp = new (Coproduct[Rng, Console, ?] ~> G) {
  def apply[A](a: Coproduct[Rng, Console, A]): G[A] =
    a.run match {
      case Left(a) => ???
      case Right(a) => ???
    }
}
```

# Interpreters

> * Assume we have a `f: Rng ~> G`, then `Left` is easy

```scala
val interp = new (Coproduct[Rng, Console, ?] ~> G) {
  def apply[A](a: Coproduct[Rng, Console, A]): G[A] =
    a.run match {
      case Left(a) => f.apply(a)
      case Right(a) => ???
    }
}
```

# Interpreters

> * Assume we have a `g: Console ~> G`, then `Right` is easy

```scala
val interp = new (Coproduct[Rng, Console, ?] ~> G) {
  def apply[A](a: Coproduct[Rng, Console, A]): G[A] =
    a.run match {
      case Left(a) => f.apply(a)
      case Right(a) => g.apply(a)
    }
}
```

# Interpreters

> * Even better, we can generalize!

```scala
def coproInterp[F[_], G[_], H[_]](f: F ~> H, g: G ~> H) =
  new (Coproduct[F, G, ?] ~> H) {
    def apply[A](a: Coproduct[F, G, A]): H[A] =
      a.run match {
        case Left(a) => f.apply(a)
        case Right(a) => g.apply(a)
      }
  }
```

# Interpreters

* Given
    * `f: Rng ~> IO`
    * `g: Console ~> IO`
* We can run `prg: Free[Coproduct[Rng, Console, ?], A]`
* `foldMap(prg)(coproInterp(f, g)) : IO[A]`

# Free monads vs mtl-style

* Free monads have some advantages
    * Order of effects isn't important
    * Ability to "inspect" the runtime and optimize
    * No `n^2` problem
* Free monads have some disadvantages
    * mtl tends to have better performance characteristics
    * Some operations are hard (impossible?) to represent

# One more thing...

* Why did we want free monads?
* But now we need to write a bunch of `Functor` instances
* They are simpler, but still...

# Yoneda lemma

* Given `def map[B](f: A => B): F[B]`
    * using `identity`, you have an `F[A]`

# Yoneda lemma

* Further, given a `F[A]` and `Functor[A]`
    * you natrually have the above `map`

# Yoneda lemma

* Encoded this is

```scala
trait Yoneda[F[_], A] {
  def map[B](f: A => B): F[B]
}
```

# Yoneda lemma

> * Also, `Yoneda[F,A]` <~> `F[A]`

```scala
def toYo[F[_]:Functor,A](fa: F[A]): Yoneda[F, A] = ???

def froYo[F[_],A](yo: Yoneda[F,A]): F[A] = ???
```

# Yoneda lemma

> * Also, `Yoneda[F,A]` <~> `F[A]`

```scala
def toYo[F[_]:Functor,A](fa: F[A]): Yoneda[F, A] =
  new Yoneda[F,A] {
    def map[B](f: A => B): F[B] = ???
  }

def froYo[F[_],A](yo: Yoneda[F,A]): F[A] = ???
```

# Yoneda lemma

> * Also, `Yoneda[F,A]` <~> `F[A]`

```scala
def toYo[F[_]:Functor,A](fa: F[A]): Yoneda[F, A] =
  new Yoneda[F,A] {
    def map[B](f: A => B): F[B] = Functor[F].map(fa)(f)
  }

def froYo[F[_],A](yo: Yoneda[F,A]): F[A] = ???
```

# Yoneda lemma

> * Also, `Yoneda[F,A]` <~> `F[A]`

```scala
def toYo[F[_]:Functor,A](fa: F[A]): Yoneda[F, A] =
  new Yoneda[F,A] {
    def map[B](f: A => B): F[B] = Functor[F].map(fa)(f)
  }

def froYo[F[_],A](yo: Yoneda[F,A]): F[A] =
  yo.map(identity)
```

# Coyoneda lemma

* the opposite is also true!
* Given an `F[A]`, for *any* `F`
* can destructure it to `F[B]` and `B => A`, for some `B`
* `F` does not need to be a `Functor`!

# Coyoneda lemma

> * Given an `F[A]`, for *any* `F` and `A`
```scala
trait Coyoneda[F[_], A]
```

# Coyoneda lemma

> * for some `B`

```scala
trait Coyoneda[F[_], A] {
  type B
}
```

# Coyoneda lemma

> * we can destructure `F[A]` into a `B => A`

```scala
trait Coyoneda[F[_], A] {
  type B
  def f: B => A
}
```

# Coyoneda lemma

> * and an `F[B]`

```scala
trait Coyoneda[F[_], A] {
  type B
  def f: B => A
  def fb: F[B]
}
```

# Coyoneda lemma

> * And `Coyoneda[F, A] <~> F[A]`

```scala
def toCoYo[F[_],A](fa: F[A]): Coyoneda[F, A] = ???

def froCoYo[F[_]:Functor,A](yo: CoYo): F[A] = ???
```

# Coyoneda lemma

> * And `Coyoneda[F, A] <~> F[A]`

```scala
def toCoYo[F[_],A](fa: F[A]) = new Coyoneda[F,A] {
  type B = A
  val f = (a: A) => a
  val fb = fa
}

def froCoYo[F[_]:Functor,A](yo: CoYo) = ???
```

# Coyoneda lemma

> * And `Coyoneda[F, A] <~> F[A]`

```scala
def toCoYo[F[_],A](fa: F[A]) = new Coyoneda[F,A] {
  type B = A
  val f = (a: A) => a
  val fb = fa
}

def froCoYo[F[_]:Functor,A](yo: CoYo) =
  Functor[F].map(yo.fi)(yo.f)
```

# Coyoneda lemma

* we can write a `Functor[Coyoneda[F, ?]` for *any* `F`

```scala
class CoYoFunctor[F[_]] extends Functor[Coyoneda[F, ?]] {
  def map[A, C](ya: Coyoneda[F, A])(g: A => C): Coyoneda[F, C] =
    ???
}
```

# Coyoneda lemma

> * only thing we need to do is compose the functions

```scala
class CoYoFunctor[F[_]] extends Functor[Coyoneda[F, ?]] {
  def map[A, C](ya: Coyoneda[F, A])(g: A => C): Coyoneda[F, C] =
    new Coyoneda[F, C] {
      type B = ya.B
      val f: B => C = ya.f andThen g
      val fb: F[B] = ya.fb
    }
}
```

# Coyoneda lemma

> * Given a `F ~> G`, we can transform `Coyoneda[F, A]` to `Coyoneda[G, A`

```scala
def coyoTrans[F[_], G[_]](f: F ~> G)(coyo: Coyoneda[F, A]): Coyoneda[G, A] =
   ???
```

# Coyoneda lemma

> * By applying `g`

```scala
def coyoTrans[F[_], G[_]](g: F ~> G)(coyo: Coyoneda[F, A]): Coyoneda[G, A] =
   new Coyoneda[G, A] {
     type B = coyo.B
     val f = coyo.f
     val fb = g.apply(coyo.fb)
   }
```

# Coyoneda lemma

> * If we have `Functor[G]`, we can write an interpreter for `Coyoneda[F, A]`

```scala
def coyoInterp[F[_], G[_]: Functor](f: F ~> G) = new (Coyoneda[F, A] ~> G) {
  def apply[A](coyo: Coyoneda[F, A]): G[A] = {
    ???
}
```

# Coyoneda lemma

> * First transform to `Coyoneda[G, A]`

```scala
def coyoInterp[F[_], G[_]: Functor](f: F ~> G) = new (Coyoneda[F, A] ~> G) {
  def apply[A](coyo: Coyoneda[F, A]): G[A] = {
    ???(coyoTrans(f))
}
```

# Coyoneda lemma

> * Grab our `G` value from our `Coyoneda[G, A]` value

```scala
def coyoInterp[F[_], G[_]: Functor](f: F ~> G) = new (Coyoneda[F, A] ~> G) {
  def apply[A](coyo: Coyoneda[F, A]): G[A] = {
    ???(coyoTrans(f).fb)
}
```

# Coyoneda lemma

> * Use `Functor[G]` to map over the `G` value...

```scala
def coyoInterp[F[_], G[_]: Functor](f: F ~> G) = new (Coyoneda[F, A] ~> G) {
  def apply[A](coyo: Coyoneda[F, A]): G[A] = {
    Functor[G].map(coyoTrans(f).fb)(???)
}
```

# Coyoneda lemma

> * using the function `coyo`, to end up with a `G[A]`

```scala
def coyoInterp[F[_], G[_]: Functor](f: F ~> G) = new (Coyoneda[F, A] ~> G) {
  def apply[A](coyo: Coyoneda[F, A]): G[A] = {
    Functor[G].map(coyoTrans(f).fb)(coyo.f)
}
```

# Coyoneda lemma

* We no longer need `Rng` and `Console` to even be functors!
* Even better, with scalaz 7.2.x, the `Free` encoding takes care of all this for us!

```scala
def liftF[S[_], A](value: S[A]): Free[S, A]
def foldMap[M[_]](f: S ~> M)(implicit M: Monad[M]): M[A]
```

# Last bits of boilerplate

* Those functions, those traits, their instances, yuck!
* Someone already wrote (most) of a library for that!
* [trivial-macros](https://bitbucket.org/atlassianlabs/trivial-macros)

# Last bits of boilerplace

> * This bit of code

```scala
import com.atlassian.id.trivial.macros.GADT
@GADT(
  ReadLn[String],
  PutStr[Unit](a: String),
) sealed trait Console[A]
```

# Last bits of boilerplace

> * Expands to

```scala
sealed trait Console[A]
object Console {
  case object ReadLn extends Console[String]
  final case class PutStr(a: String) extends Console[Unit]

  def readLn[F[_]](implicit F: Inject[Console, F]): Free[F, String] = ...
  def putStr[F[_]](s: String)(implicit F: Inject[Console, F]): Free[F, Unit] = ...
}
```

# Last bits of boilerplate

* The boring parts are automated!
* Freeing us to write and compose interpreters!

# Fin

```
 _____                _   _                ___
|  _  |              | | (_)              |__ \
| | | |_   _  ___ ___| |_ _  ___  _ __  ___  ) |
| | | | | | |/ _ / __| __| |/ _ \| '_ \/ __|/ /
\ \/' | |_| |  __\__ | |_| | (_) | | | \__ |_|
 \_/\_\\__,_|\___|___/\__|_|\___/|_| |_|___(_)
```

