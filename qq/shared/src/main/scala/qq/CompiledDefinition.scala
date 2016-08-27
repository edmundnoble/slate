package qq

import qq.QQCompiler.{CompiledFilter, OrCompilationError}
import scalaz.syntax.either._

final case class CompiledDefinition[AnyTy]
(name: String, numParams: Int, body: (List[CompiledFilter[AnyTy]] => OrCompilationError[CompiledFilter[AnyTy]]))

object CompiledDefinition {
  def undefinedOnPlatform[AnyTy](name: String) = CompiledDefinition[AnyTy](name, 0, body = _ => UndefinedOnPlatform(name).left)

  final def noParamDefinition[AnyTy](name: String, fun: CompiledFilter[AnyTy]): CompiledDefinition[AnyTy] = {
    CompiledDefinition[AnyTy](
      name,
      numParams = 0,
      _ => fun.right[QQCompilationException]
    )
  }
}

