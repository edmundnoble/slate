import matryoshka.Fix

package object qq {
  // a filter AST which is not being streamed
  type ConcreteFilter = Fix[FilterComponent]
}
