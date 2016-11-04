package slate

import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

package object app {
  def inj[C <: Coproduct]: InjPartialApply[C] = new InjPartialApply[C]
}

final class InjPartialApply[C <: Coproduct] {
  def apply[I](i: I)(implicit Inj: Inject[C, I]): C = Inj(i)
}
