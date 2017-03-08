package slate

import shapeless.Coproduct
import shapeless.ops.coproduct.{Basis, Inject}

package object app {
  def copInj[C <: Coproduct]: InjPartialApply[C] = new InjPartialApply[C]
  def copSub[Sup <: Coproduct]: BasisPartialApply[Sup] = new BasisPartialApply[Sup]
}

final class InjPartialApply[C <: Coproduct] {
  def apply[I](i: I)(implicit Inj: Inject[C, I]): C = Inj(i)
}

final class BasisPartialApply[Sup <: Coproduct] {
  def apply[Sub <: Coproduct](i: Sub)(implicit B: Basis[Sup, Sub]): Sup = B.inverse(Right(i))
}
