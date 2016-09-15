import matryoshka.{Fix, Mu}

import scala.language.higherKinds

package object qq {
  type FilterRec[T[_[_]]] = T[FilterComponent]

  type ConcreteFilter = FilterRec[Fix]

  implicit class ToDefinitions[F](defs: Seq[Definition[F]]) {
    @inline def toDefinitions: Program.Definitions[F] =
      defs.map(d => d.name -> d)(collection.breakOut)
  }

}
