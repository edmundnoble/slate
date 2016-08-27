package dash.app

import dash.identify
import qq.CompiledDefinition

object DashPrelude {

  import CompiledDefinition.noParamDefinition

  def googleAuth: CompiledDefinition[Any] = noParamDefinition("googleAuth", _ => identify.getAuthToken(interactive = false).map(_ :: Nil))

  val all = List(googleAuth)
}
