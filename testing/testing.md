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
(a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)  

## Identity
a ⊕ ∅ = ∅ ⊕ a

# Code time!

# What about side-effecting computations?

- Generalize to abstract side effects away

~~~
trait Wibble[M[_]] {
  def createWobble(name: String): M[Wobble]
  def findWobble(name: String): M[Option[Wobble]]
}
~~~

# Other Talks

> - [Introduction to Laws](http://vimeo.com/58236838) by Nick Partridge - [slides](http://www.slideshare.net/nkpart/introduction-to-laws)

# Questions?
![](graphics/questions.png "Questions?")

