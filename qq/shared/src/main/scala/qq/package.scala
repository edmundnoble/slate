import matryoshka.Fix

package object qq {
  type Filter = Fix[FilterComponent]

  implicit class ToDefinitions(defs: Seq[Definition]) {
    @inline def toDefinitions: Program.Definitions =
      defs.map(d => d.name -> d)(collection.breakOut)
  }

}
