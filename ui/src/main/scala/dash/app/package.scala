package dash

import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

package object app {

  def inj[C <: Coproduct, I](i: I)(implicit Inj: Inject[C, I]): C = Inj(i)

}
