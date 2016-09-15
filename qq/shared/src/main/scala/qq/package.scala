import matryoshka.{Fix, Mu}

import scala.language.higherKinds

package object qq {
  type ConcreteFilter = Fix[FilterComponent]

  implicit class ToDefinitions[F](defs: Seq[Definition[F]]) {
    @inline def toDefinitions: Program.Definitions[F] =
      defs.map(d => d.name -> d)(collection.breakOut)
  }
}
