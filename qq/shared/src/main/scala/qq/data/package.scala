package qq

import qq.data.ast.FilterComponent
import qq.util.Fix

package object data {
  type FilterAST = Fix[FilterComponent]
}
