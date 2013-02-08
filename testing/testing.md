% Testing
% Richard Wallace

# Laws

![](graphics/IAMTHELAW.jpg "I AM THE LAW")

- Equivalence relations
- The "contract" for a type-class
- Types cannot enforce laws
- Implementations can be wrong!

# Monoid laws

## Associativity
(a + b) + c = a + (b + c)  

## Identity
a + ∅ = ∅ + a = a

# Code time!

# What about "impure" computations?

> - Generalize to abstract side effects away

~~~
trait Wibble[M[_]] {
  def createWobble(name: String): M[Wobble]
  def findWobble(name: String): M[Option[Wobble]]
}
~~~

- What laws can we write for Wibble?
- How can we do it without side-effects?

# Code break!

# Testing functions that use Wibble

~~~
def createWobbleUnlessExists[M[_]: Monad : Wibble](name: String): M[Wobble] =
  findWobble(name) >>= (_.map(_.point[M]).getOrElse(createWobble(name)))
~~~

- This is a pure function.
- Testing pure functions is easy!
- Just pass in the initial value and check the result.

# More code

# Stacking State to build behavior

~~~
trait Bippy[M[_]] {
  def findBoppy(name: String): M[Option[Boppy]]
}

def both[M[_]: Monad : Wibble : Bippy](name: String): M[Option[(Wobble, Boppy)]]
~~~

- How do we test `both`?

# Solution #1

> - Build an `M` manually

~~~
case class WibbleBippyStateT[M[+_], A](s: StateT[M, (List[Wobble], List[Boppy]), A])
type WibbleBippyState[A] = WibbleBippyStateT[Id, A]

implicit val WibbleBippyStateInstances = new Monad[..] with Wibble[..] with Bippy[..] {
  ...
}
~~~

# Solution #2

> - Build an `M` by stacking State

~~~
case class WibbleStateT[M[+_], A](s: StateT[M, List[Wobble], A])
implicit def WibbleStateTinstances[M[_]](implicit M: Monad[M]) = new Monad[..] with Wibble[..] {
  ...
}

case class BippyStateT[M[+_], A](s: StateT[M, List[Boppy], A])
implicit def BippyStateTinstances[M[_]](implicit M: Monad[M]) = new Monad[..] with Bippy[..] {
  ...
}

implicit def WibbleStateTDerivedInstance[M[_], S](implicit M: Wibble[M]) =
  new Wibble[({ type λ[α] = StateT[M, S, α]})#λ] {
  ...
}

type WibbleBippyStateT[M[_], A] = BippyStateT[({ type λ[α] = WibbleStateT[M, α]})#λ, A]
type WibbleBippyState[A] = WibbleBippyStateT[Id, A]
~~~

# Pros/cons

## Manual 
### Pros
- Single run/exec/eval
- Easy to add new behavior

### Cons
- Lots of boilerplate
- Monolithic

## Stacking
### Pros
- Easy to add new behavior
- Get only the behavior you need
- Code reuse

### Cons
- Run multiple evals to get result - `o.exec(s0).exec(s1).exec(s2)`

# Which to use?

- Why not both?
- Core application logic - manual construction
- Need a web layer? Stack it

# How's it look?

~~~
case class WibbleBippyStateT[M[+_], A](s: StateT[M, TestData, A])
implicit val WibbleBippyStateInstances = new Monad[..] with Wibble[..] with Bippy[..] {}

case class WebStateT[M[+_], A](s: StateT[M, ReqResData, WebResult[A])
implicit val WebStateTInstances = new Monad[..] with Web[..] {}

type ResState[A] = WibbleBippyStateT[({type λ[α] = WebStateT[Id, α]})#λ, A]
~~~ 

- Web resources are run in the `ResState` monad
- Evaluate by running `rs.eval(TestData()).eval(ReqResData())`
- Compare final state of `TestData` and `ReqResData` against expected values

# Other Talks

> - [Introduction to Laws](http://vimeo.com/58236838) by Nick Partridge - [slides](http://www.slideshare.net/nkpart/introduction-to-laws)
> - [Scalaz State Monad](http://www.youtube.com/watch?v=Jg3Uv_YWJqI) by Michael Pilquist - [slides](https://speakerdeck.com/mpilquist/scalaz-state-monad)

# Questions?
![](graphics/questions.png "Questions?")

