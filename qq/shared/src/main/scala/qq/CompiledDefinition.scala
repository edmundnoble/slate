package qq

import qq.QQCompiler.{CompiledFilter, OrCompilationError}
import scalaz.syntax.either._

final case class CompiledDefinition[JsonTy]
(name: String, numParams: Int, body: (List[CompiledFilter[JsonTy]] => OrCompilationError[CompiledFilter[JsonTy]]))

object CompiledDefinition {
  def undefinedOnPlatform[JsonTy](name: String) = CompiledDefinition[JsonTy](name, 0, body = _ => UndefinedOnPlatform(name).left)

  final def noParamDefinition[JsonTy](name: String, fun: CompiledFilter[JsonTy]): CompiledDefinition[JsonTy] = {
    CompiledDefinition[JsonTy](
      name,
      numParams = 0,
      _ => fun.right[QQCompilationException]
    )
  }
}

