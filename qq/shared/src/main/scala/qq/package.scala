import matryoshka.Fix

package object qq {
  type ConcreteFilter = Fix[FilterComponent]
}
