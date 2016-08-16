package qq

import qq.Recursion.RecursionEngine

object Platform {
  object Rec {
    implicit val defaultRecScheme: RecursionEngine =
      Recursion.Unsafe.LimitStack(maxStackSize = 50)
  }
}
