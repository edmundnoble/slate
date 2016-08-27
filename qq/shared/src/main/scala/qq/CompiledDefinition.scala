package qq

import qq.QQCompiler.{CompiledFilter, OrCompilationError}
import scalaz.syntax.either._

final case class CompiledDefinition[AnyTy]
(name: String, numParams: Int, body: (List[CompiledFilter[AnyTy]] => OrCompilationError[CompiledFilter[AnyTy]]))

object CompiledDefinition {
  def undefinedOnPlatform[AnyTy](name: String) = CompiledDefinition[AnyTy](name, 0, body = _ => UndefinedOnPlatform(name).left)
}

