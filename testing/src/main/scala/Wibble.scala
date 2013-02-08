package purefn.testing

import scalaz._
import syntax.monad._

case class Wobble(id: Long, name: String)

trait Wibble[M[_]] {
  def createWobble(name: String): M[Wobble]
  def findWobble(name: String): M[Option[Wobble]]
}

object Wibble {
  def createWobble[M[_]](name: String)(implicit M: Wibble[M]): M[Wobble] =
    M.createWobble(name)

  def findWobble[M[_]](name: String)(implicit M: Wibble[M]): M[Option[Wobble]] =
    M.findWobble(name)

  def createWobbleUnlessExists[M[_]: Monad : Wibble](name: String): M[Wobble] =
    findWobble(name) >>= (_.map(_.point[M]).getOrElse(createWobble(name)))
}
