package qq

import scala.language.higherKinds
import scalaz.syntax.functor._
import scalaz.std.map._
import scalaz.Functor

object Program {
  type Definitions[F] = scalaz.std.map.XMap[String, Definition[F]]

  implicit private final val definitionsFunctor: Functor[Definitions] =
    mapInstance[String].compose(Definition.definitionFunctor)
  implicit def programFunctor: Functor[Program] = new Functor[Program] {
    override def map[A, B](fa: Program[A])(f: (A) => B): Program[B] = Program[B](definitionsFunctor.map(fa.defns)(f), f(fa.main))
  }
}

case class Program[F](defns: Program.Definitions[F], main: F)

