package qq

import scalaz.Functor

// parameters are represented as strings for now, sorry performance
final case class Definition[F](name: String,
                               params: List[String],
                               body: F)

object Definition {
  implicit val definitionFunctor: Functor[Definition] = new Functor[Definition] {
    override def map[A, B](fa: Definition[A])(f: (A) => B): Definition[B] =
      Definition[B](fa.name, fa.params, f(fa.body))
  }
}
