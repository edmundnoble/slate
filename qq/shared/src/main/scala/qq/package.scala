import matryoshka._

package object qq {
  type Filter = Fix[FilterComponent]
}
