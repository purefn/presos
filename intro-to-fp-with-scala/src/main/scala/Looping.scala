object Looping {
  def sum(xs: List[Int]): Int = {
    var s = 0
    for (x <- xs) s = s + x
    s
  }
  
  def main(args : Array[String]): Unit = {
    println(sum(List(1, 2, 3)))
  }
}


















//  def concatStrings(xs: List[String]): String = {
//    var s = ""
//    for (x <- xs) s = s + x
//    s
//  }
//  
//  def concatLists(xs: List[List[Int]]): List[Int] = {
//    var s = List[Int]()
//    for (x <- xs) s = s ++ x
//    s
//  }
//  
//
//  def mergeMaps(xs: Set[Map[String, Int]]): Map[String, Int] = {
//    var m = Map[String, Int]()
//    for (x <- xs) {
//      for (p <- x) m = m + ((p._1, m.getOrElse(p._1, 0) + p._2))
//    }
//    m
//  }

//  def foldl(xs: List[Int], init: Int)(f: (Int, Int) => Int): Int =
//    xs match {
//      case Nil => init
//      case h :: t => foldl(t, f(init, h))(f)
//    }
//  def sum(xs: List[Int]): Int = foldl(xs, 0) { (a, b) => a + b }

//  def sum(xs: List[Int]): Int = xs.foldLeft(0) { (a, b) => a + b }
//                                xs.foldLeft(0) { _ + _ }

//  object IntMonoid {
//    val zero = 0
//    def append(x: Int, y: Int) = x + y
//  }

//  trait Monoid[A] {
//    def zero: A
//    def append(x: A, y: A)
//  }

// def sum(xs: List[Int], m: Monoid[Int]) = xs.foldLeft(m.zero)(m.append)

// def sum[A](xs: List[A], m: Monoid[A]) = xs.foldLeft(m.zero)(m.append)

// def sum[A](xs: List[A])(implicit m: Monoid[A]) = xs.foldLeft(m.zero)(m.append)

// object ListFoldLeft {
//   def foldLeft(xs: List[Int], init: Int, f: (Int, Int) => Int) = xs.foldLeft(init)(f)
// }

// trait Foldl[F[_]] {
//   def foldl[A, B](xs: F[A], init: B, f: (B, A) => B): B
// }

// def sum[F[_], A](xs: F[A])(implicit m: Monoid[A], fl: Foldl[F]) = fl.foldl(xs, m.zero, m.append)

// sealed trait Tree[A]
// case class Leaf[A](a: A) extends Tree[A]
// case class Bin[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// implicit object TreeFoldl extends Foldl[Tree] {
//   def foldl[A, B](xs: Tree[A], init: B, f: (B, A) => B) = 
//     xs match {
//       case Leaf(a) => f(init, a)
//       case Bin(left, right) => foldl(right, foldl(left, init, f), f)
//     }
// }

// trait Identity[A] {
//   val value: A
//   def |+|(a: A)(implicit m: Monoid[A]) = m.append(value, a)
// }
// 
// object Identity {
//   implicit def wrapIdentity[A](a: A) = new Identity[A] { val value = a }
// }

// trait MA[M[_], A] {
//   val value: M[A]
//   def sum(implicit m: Monoid[A], fl: Foldl[M]) = fl.foldl(value, m.zero, m.append)
// }
//
// object MA {
//   implicit def wrapMA[M[_], A](ma: M[A]) = new MA[M, A] { val value = ma }
// }
