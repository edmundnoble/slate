package qq
package data

import qq.cc.{CompiledFilter, OrCompilationError, QQCompilationException, UndefinedOnPlatform}

import scalaz.syntax.either._

final case class CompiledDefinition[J]
(name: String, numParams: Int, body: (List[CompiledFilter[J]] => OrCompilationError[CompiledFilter[J]]))

object CompiledDefinition {
  def undefinedOnPlatform[J](name: String): CompiledDefinition[J] =
    CompiledDefinition[J](name, 0, body = _ => UndefinedOnPlatform(name).left)

  final def noParamDefinition[J](name: String, fun: CompiledFilter[J]): CompiledDefinition[J] = {
    CompiledDefinition[J](
      name,
      numParams = 0,
      _ => fun.right[QQCompilationException]
    )
  }
}

