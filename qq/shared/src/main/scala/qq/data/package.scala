package qq

import qq.util.Fix

package object data {
  // a filter AST which is not being streamed
  type ConcreteFilter = Fix[FilterComponent]
}
