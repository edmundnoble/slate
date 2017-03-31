package qq

import qq.ast.FilterComponent
import qq.util.Fix

package object data {
  type FilterAST = Fix[FilterComponent]
}
