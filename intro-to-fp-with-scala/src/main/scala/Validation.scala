import scalaz._, Scalaz._

object ValidationExample {
  def ??? = sys.error("not implemented")

  case class Person(name: String, age: Int)

  def validateAge(a: Int) = ???

  def validateName(s: String) = ???

  def mkPerson(name: String, age: Int) = ???

  def main(args: Array[String]) {
    println(mkPerson("Andrew", 5))

    println(mkPerson("A" * 131, 15))

    println(mkPerson("Methuselah", 969))

    println(mkPerson("methuselah", 969))
  }
}






























def validateAge(a: Int): Validation[String, Int] =
  if (0 to 130 contains a) a.success
  else "Age must be in range".fail

def validateName(s: String): Validation[String, String] =
  if (s.headOption.exists(_.isUpper)) s.success
  else "Name must start with a capital letter".fail

def mkPerson(name: String, age: Int): Validation[String, Person] =
  (validateName(name) |@| validateAge(age)){ (n, a) => Person(n, a)}

