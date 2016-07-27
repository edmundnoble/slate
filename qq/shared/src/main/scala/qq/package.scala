import matryoshka.Fix

package object qq {
  type Filter = Fix[FilterComponent]
}
