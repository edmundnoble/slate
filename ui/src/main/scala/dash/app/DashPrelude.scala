package dash
package app

import qq.CompiledDefinition

object DashPrelude {

  import CompiledDefinition.noParamDefinition

  def googleAuth: CompiledDefinition[Any] =
    noParamDefinition("googleAuth", _ => identify.getAuthToken(interactive = true).map(_ :: Nil))

  val all = googleAuth +: Vector.empty
}
