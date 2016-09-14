import matryoshka.Fix

package object qq {
  type Filter = Fix[FilterComponent]

  implicit class ToDefinitions[F](defs: Seq[Definition[F]]) {
    @inline def toDefinitions: Program.Definitions[F] =
      defs.map(d => d.name -> d)(collection.breakOut)
  }

}
