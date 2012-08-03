% Little Languages
% Richard Wallace

# What's the problem?
We'd like to

- Define a type
- Add new cases
- Add new functions
- Not have to recompile everything

# OO Example
```
trait Expr {
  def eval: Int
}

case class Val(eval: Int) extends Expr

case class Add(x: Expr, y: Expr) extends Expr {
  lazy val eval = x.eval + y.eval
}
```

> - Easy to add new types of `Expr`
> - Hard to add new functions

# FP Example
```
data Expr = Val Int | Add Expr Expr

eval :: Expr → Int
eval (Val x) = x
eval (Add x y) = eval x + eval y
```

> - Easy to add new functions
> - Hard to add new types of `Expr`

# The Expression Problem
> ... is a new name for an old problem. The goal is to define a datatype by cases, where one can add new cases to the datatype and new functions over the datatype, without recompiling existing code, and while retaining static type safety (e.g., no casts) - Philip Wadler

# Data types à la carte
Let's try building a more generic expression. As a first pass, consider:

```
data Expr f = In (f (Expr f))
```

> - `f` is the type constructor
> - `Expr` is a tree of `f`s

# Example
```
data Val e = Val Int
type IntExpr = Expr Val

one :: IntExpr
one = In (Val 1)
```

> - `IntExpr` is the simplest possible expression
> - It has no subtrees

# Another example
```
data Add e = Add e e
type AddExpr = Expr Add

(⊕) :: Expr e → Expr e → AddExpr
e ⊕ e = In (Add e e)
```

> - `AddExpr` is a tree of `Add`s
> - Impossible to actually construct as-is

# Let our expressions combine!
![](graphics/Captain-Lambda.png "Captain Lambda")

- Combine using coproducts
- Coproduct?
    - Dual of product
- Product?
    - aka `Tuple`
    - `(X, Y, Z) ⇒ X and Y and Z`
- Dual of `X and and Y and Z` is...
    - `X or Y or Z ⇒`
    - `Either`!

# Expressions with coproducts
```
data (f :+: g) e = Inl (f e) | Inr (g e)

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))
```

- Yikes! We'll cleanup `addExample` later, but it's useful to look at the tree
- `Expr (Val :+: Add)` says 
    - an expression can be constructed from values and additions

# These are all Functors!
```
instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor, f, Functor g) ⇒ Functor (f :+: g) where
  fmap f (Inl e1) ⇒ Inl (fmap f e1)
  fmap f (Inr e2) ⇒ Inr (fmap f e2)
```

# Evaluation
```
foldExpr :: Functor f ⇒ (f a → a) → Expr f → a
foldExpr fn (In t) = fn (fmap (foldExpr fn) t)
```

- The function `fn` specifies _algebra_ for evaluating `Expr f`

# Defining our algebra
```
class Functor f ⇒ Eval f where
  evalAlgebra :: f Int → Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) ⇒ Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y
```

# Evaluate
```
eval :: Eval f ⇒ Expr f → Int
eval expr = foldExpr evalAlgebra expr
```

Let's evaluate addExample

- `eval addExample` ⇒
- `eval (In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219))))))` ⇒
- `foldExpr evalAlgebra (In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219))))))` ⇒
- `evalAlgebra (fmap (foldExpr evalAlgebra) (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219))))))` ⇒

# eval addExample (con't)
- `evalAlgebra (Inr (foldExpr evalAlgebra (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))))` ⇒
- `evalAlgebra (Inr (evalAlgebra (fmap (foldExpr evalAlgebra) (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219))))))))` ⇒
- `evalAlgebra (Inr (evalAlgebra (Inr (fmap (foldExpr evalAlgebra) (Add (In (Inl (Val 118))) (In (Inl (Val 1219))))))))` ⇒
- `evalAlgebra (Inr (evalAlgebra (Inr (Add (foldExpr evalAlgebra (In (Inl (Val 118)))) (foldExpr evalAlgebra (In (Inl (Val 1219))))))))`

# eval addExample (con't)

- `evalAlgebra (Inr (evalAlgebra (Inr (Add (evalAlgebra (fmap (foldExpr evalAlegbra) (Inl (Val 118)))) (evalAlgebra (fmap (foldExpr evalAlgebra) (Inl (Val 1219))))))))` ⇒
- `evalAlgebra (Inr (evalAlgebra (Inr (Add (evalAlgebra (Inl (fmap (foldExpr evalAlgebra) (Val 118)))) (evalAlgebra (Inl (fmap (foldExpr evalAlgebra) (Val 1219))))))))` ⇒
- `evalAlgebra (Inr (evalAlgebra (Inr (Add (evalAlgebra (Inl (Val 118))) (evalAlgebra (Inl (Val 1219)))))))` ⇒

# eval addExample (con't)

- `evalAlgebra (Inr (evalAlgebra (Inr (Add (evalAlgebra (Val 118)) (evalAlgebra (Val 1219))))))` ⇒
- `evalAlgebra (Inr (evalAlgebra (Inr (Add 118 1219))))` ⇒
- `evalAlgebra (Inr (evalAlgebra (Add 118 1219)))` ⇒
- `evalAlgebra (Inr 1337)` ⇒
- `1337`

# Cleanup time!
![](graphics/sweeper.png "Sweeper")

As a first pass

```
val :: Int → Expr Val
val x = In (Val x)

(⊕) :: Expr Add → Expr Add → Expr Add
x ⊕ y = In (Add x y)

val 1 ⊕ val 3 -- Does not compile!
```

- Type of `⊕` is fixed as `Expr Add`.
- It needs to be `Expr (Val :+: Add)`.

# Injections

- We'd like something like

```
val :: Int → Expr f
```

- But we need a way to *prove* that the algebra for `f` supports `Val`

```
val :: (Val :<: f) ⇒ Int → Expr f
(⊕) :: (Add :<: f) ⇒ Expr f → Expr f → Expr f
```

- `:<:` can be read as "any `f` that supports addition"

```
class (Functor sub, Functor sup) ⇒ sub :<: sup where
  inj :: sub a → sup a
```

# Injections (con't)
```
instance Functor f ⇒ f :<: f where
  inj = id

instance (Functor f, Functor g) ⇒ f :<: (f :+: g) where
  inj = Inl

inject :: (g :<: f) ⇒ g (Expr f) → Expr f
inject = In . inj

val :: (Val :<: f) ⇒ Int → Expr f
val x = inject (Val x)

(⊕) :: (Add :<: f) ⇒ Expr f → Expr f → Expr f
x ⊕ y = inject (Add x y)

let x :: Expr (Add :+: Val) = val 30000 ⊕ val 1330 ⊕ 7
eval x  -- 31337
```

# The payoff
![](graphics/payoff.png "Payoff")

Yay we can add numbers, but also...

```
data Mul x = Mul x x

instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

(⊗) :: (Mul :<: f) ⇒ Expr f → Expr f → Expr f
x ⊗ y = inject (Mul x y)
```

Now we can write

```
> let x :: Expr (Val :+: Add :+: Mul) = val 80 ⊗ val 5 ⊕ val 4
> eval x
404
```

# Now in Scala
```
sealed case class Expr[F[_]](e: F[Expr[F]])
sealed case class Val[+A](v: Int)
sealed case class Add[+A](x: A, y: A)
sealed case class Mult[+A](x: A, y: A)

implicit lazy val ValFunctor = new Functor[Val] {
  def map[A, B](a: Val[A])(f: A ⇒ B) =
    Val[B](a.v)
}

implicit lazy val AddFunctor = new Functor[Add] {
  def map[A, B](a: Add[A])(f: A ⇒ B) =
    Add[B](f(a.x), f(a.y))
}
// Functor[Mult] is pratically the same as Functor[Add]
```

# What about `:+:` ???
```
type Coproduct[F[_], G[_], A] = Either[F, G, A]

type ValAddMult[A] = 
  Coproduct[Val, ({type λ[α]=Coproduct[Add, Mult, α]}), A]
```

We can use some tricks to get something better

```
trait Coproduct[F[_], G[_]] { 
  type or[H[_]] = 
    Coproduct[F, ({type λ[α] = Either[G[α], H[α]]})#λ]
  type ap[A] = Either[F[A], G[A]]
}

type ValAddMult[A] = coproduct[Val, Add]#or[Mult]#ap[A]
```

Still not as nice, but this is Scala

# Coproduct Functor
```
implicit def CoproductFunctor[F[_]: Functor, G[_]: Functor] = 
  new Functor[Coproduct[F, G]#ap] {
    def map[A, B](a: Either[F[A], G[A]])(f: A ⇒ B) = 
      a match {
        case Left(x)  ⇒ Left(Functor[F].map(x)(f))
        case Right(x) ⇒ Right(Functor[G].map(x)(f))
      }
  }
```

- Are we there? Sadly, no.
    - `implicitly[Functor[ValAdd]]` works fine, but...
    - `implicitly[Functor[ValAddMult]]` leads to "cannot find implicit" :(

# Manual coproduct creation
![](graphics/Manual-Labor.png "Manual Labor")

```
sealed trait ValAddMult[+A]
sealed case class ValTerm[A](t: Val[A]) extends ValAddMult[A]
sealed case class AddTerm[A](t: Add[A]) extends ValAddMult[A]
sealed case class MultTerm[A](t: Mult[A]) extends ValAddMult[A]

implicit def ValAddMultFunctor = new Functor[ValAddMult] {
  def map[A, B](a: ValAddMult[A])(f: A ⇒ B) = a match {
    case ValTerm(t)  ⇒ ValTerm(Functor[Val].map(t)(f))
    case AddTerm(t)  ⇒ AddTerm(Functor[Add].map(t)(f))
    case MultTerm(t) ⇒ MultTerm(Functor[Mult].map(t)(f))
  }
}
```

# Evaluation in Scala
```
def foldExpr[F[_]: Functor, A](e: Expr[F])(f: F[A] ⇒ A): A =
  f(Functor[F].map(e.e)(foldExpr(_)(f)))

sealed trait Eval[F[_]] {
  def evalAlgebra(e: F[Int]): Int
}

implicit lazy val ValEval = new Eval[Val] {
  def evalAlgebra(v: Val[Int]) = v.v
}

implicit lazy val AddEval = new Eval[Add] {
  def evalAlgebra(a: Add[Int]) = a.x + a.y
}

implicit lazy val ValAddMultEval = new Eval[ValAddMult] {
  def evalAlgebra(e: ValAddMult[Int]) = e match {
    case ValTerm(t)  ⇒ implicitly[Eval[Val]].evalAlgebra(t)
    case AddTerm(t)  ⇒ implicitly[Eval[Add]].evalAlgebra(t)
    case MultTerm(t) ⇒ implicitly[Eval[Mult]].evalAlgebra(t)
  }
}  

def eval[F[_]: Eval: Functor](e: Expr[F]): Int =
  foldExpr(e)(implicitly[Eval[F]].evalAlgebra)
```

# Injections in Scala
```
trait Injection[F[_], G[_]] {
  def inj[A](fa: F[A]): G[A]
}

type :<:[F[_], G[_]] = Injection[F, G]
```

More manual labor

```
implicit lazy val ValInjValAddMult = new (Val :<: ValAddMult) {
  def inj[A](v: Val[A]) = ValTerm(v)
}
// Other instances omitted. Just boilerplate.

def value[F[+_]](v: Int)(implicit F: Functor[F], FG: Val :<: F): F[Int] =
  FG.inj(Val(v))
```

# Free Your Monads
![](graphics/FreeMind.png "Free Your Mind")

- Now what about...
    - storing results?
    - using GPUs for matrix or vector operations?
- Impure computations

# Free Monad
```
data Free f a = Pure a | Impure (f (Free f a))

instance Functor f ⇒ Functor (Free f) where
  fmap fn (Pure a)   ⇒ Pure (fn x)
  fmap fn (Impure t) ⇒ Impure (fmap (fmap fn) t)

instance Functor f ⇒ Monad (Free f) where
  return x         ⇒ Pure x
  (Pure x) >>= f   ⇒ f x
  (Impure t) >>= f ⇒ Impure (fmap (>>= f) t)
```

- `Free f a` is either a "pure" value or some "impure" computation in `f`
- `Free f a` is a `Monad` if `f` is a `Functor`

# Free Monad (con't)
- `Free f a` is a tree
- What if we use `(,) a` for `f`?
    - `Impure ((0, Impure((1, Pure 2))))`
    - `%s/Impure/Cons/g`
    - Cons((0, Cons ((1, Pure 2))))
    - `[a]`

# Evaluation
```
foldFree :: Functor f ⇒ (a → b) → (f b → b) → Free f a → b
foldFree pure imp (Pure x) = pure x
foldFree pure imp (Impure t) = imp (fmap (foldFree pure imp) t)
```

- Similar to `foldExpr` we saw before
- `pure` transforms leaf values
- `impure` is our algebra

# A Pure example of Impurity
```
data Console a =
    GetChar (Char → a)
  | PutChar Char a
data FileSytem a =
    ReadFile FilePath (String → a)
  | WriteFile FilePath String a

exec :: Exec f ⇒ Free f a → IO a
exec = foldFree return execAlgebra

class Functor f ⇒ Exec f where
  execAlgebra :: f (IO a) → IO a
instance Exec Console where
  execAlgebra (GetChar f)    = Prelude.getChar >>= f
  execAlgebra (PurChar c io) = Prelude.putChar c >>= io
```

# A Pure example of Impurity (con't)
```
cat :: FilePath → Free (Console :+: FileSystem) ()
cat fp = do
  contents ← readFile fp
  mapM putChar contents -- mapM :: (a → m b) → [a] → m [b]
  return ()
```

- Entirely pure!
- Testable!
- We can use any algebra we like!

# In Scala
```
trait Free[F[+_], +A]
sealed case class Pure[F[+_], +A](a: A) extends  Free[F, A]
sealed case class Impure[F[+_], +A](a: F[Free[F,  A]]) extends Free[F, A]

implicit def FreeFunctor[F[+_]: Functor] = new Functor[({type λ[α]=Free[F,α]})#λ] {
  def map[A, B](a: Free[F, A])(f: A ⇒ B) = a match {
    case Pure(a)   ⇒ Pure(f(a))
    case Impure(t) ⇒ Impure(Functor[F].map(t)(map(_)(f)))
  }
}

implicit def FreeMonad[F[+_]: Functor] = new Monad[({type λ[α]=Free[F,α]})#λ] {
  def point[A](a: A) = Pure(a)
  def bind[A, B](a: Free[F, A])(f: A ⇒ F[B]) = a match {
    case Pure(x)   ⇒ f(x)
    case Impure(t) ⇒ Impure(Functor[F].map(t)(bind(_)(f)))
  }
}
```

# In Scala (con't)
```
trait Console[+A]
sealed case class GetChar[+A](f: Char ⇒ A) extends Console[A]
sealed case class PutChar[+A](c: Char, f: Char ⇒ A) extends Console[A]

trait FileSystem[+A]
sealed case class ReadFile[+A](fp: String, f: String ⇒ A) extends FileSystem[A]
sealed case class WriteFile[+A](s: String, a: A) extends FileSystem[A]

def readFile[F[+_]](fp: String)(implicit F: Functor[F], FG: FileSystem :<: F): Free[F, String] =
  Impure(FG.inj(ReadFile(fp, Pure(_))))

def foldFree[F[+_]: Functor, A, B](f: Free[F, A], pure: A ⇒ B, impure: F[B] ⇒ B): B = 
  f match {
    case Pure(x) = pure(x)
    case Impure(t) = impure(Functor[F].map(t)(foldFree(_, pure, impure)))
  }

def exec[F[+_]: Exec, A](f: Free[F, A]): IO[A] =
  foldFree(_.point[IO], implicitly[Exec[F]].execAlgrebra)
```

# In Scala (con't)
```
def cat(fp: String): Free[ConsoleFileSystem, Unit] =
  for {
    contents ← readFile(fp)
    _        ← contents.toStream.mapM(putChar)
  } yield ()
```

- We could make `cat` more generic
- `def cat[F[+_]](fp: String)(implicit F: Functor, FC: Console :<: F, FF: FileSystem :<: F): Free[F, Unit]`
- Compiler will fall over at call sites unless we explicitly specify the type
- `val c: Free[ConsoleFileSystem, Unit] = cat(fp)` works
- `val c = cat[ConsoleFileSystem](fp)` also works
- `val c = cat(fp)` causes `StackOverflowError`

# Want more? How 'bout DI?
```
sealed trait Config[+A]
sealed case class GetBaseUri[+A](f: URI ⇒ A) extends Config[A]
sealed case class IsCachingEnabled[+A](f: Boolean ⇒ A) extends Config[A]
```

- Similar to `getChar`
- We want a value and we want to do something with it

# In Review
![](graphics/confused.jpg "Confused?")

- Solved the Expression Problem using coproducts to build little languages
    - In Haskell, we did this with the `:+:` type constructor and used `:<:` to build expressions
    - In Scala, we have to manually build the ADTs and specify how to combine them
- Made impure computations pure
- Injected dependencies

# Questions?
![](graphics/questions.png "Questions?")

