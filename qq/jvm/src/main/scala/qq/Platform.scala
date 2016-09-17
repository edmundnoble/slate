package qq

import qq.util.Recursion
import qq.util.Recursion.RecursionEngine

object Platform {
  object Rec {
    implicit val defaultRecScheme: RecursionEngine =
      Recursion.Unsafe.LimitStack(maxStackSize = 50)
  }
}
