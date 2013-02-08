package purefn.testing

trait Monoid[A] {
  def append(x: A, y: A): A
  def zero: A
}

object Monoid {
  implicit lazy val IntMonoid: Monoid[Int] = new Monoid[Int] {
    def append(x: Int, y: Int) = x + y
    def zero = 0
  }

  implicit def ListMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def append(xs: List[A], ys: List[A]) = xs ++ ys
    def zero = Nil
  }

  trait MonoidOps[A] {
    def self: A
    def A: Monoid[A]

    final def mappend(other: A): A = A.append(self, other)
    final def |+|(other: A): A = A.append(self, other)
  }

  implicit def ToMonoidOps[A](a: A)(implicit A0: Monoid[A]): MonoidOps[A] = new MonoidOps[A] {
    val self = a
    val A = A0
  }

  def mzero[A](implicit A: Monoid[A]): A = A.zero
  def ∅[A](implicit A: Monoid[A]): A = A.zero
}
