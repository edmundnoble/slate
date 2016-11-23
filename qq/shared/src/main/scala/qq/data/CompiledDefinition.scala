package qq
package data

import cats.implicits._
import org.atnos.eff.syntax.all._
import qq.cc._

final case class CompiledDefinition
(name: String, numParams: Int, body: (Vector[CompiledFilter] => OrCompilationError[CompiledFilter]))

object CompiledDefinition {
  def undefinedOnPlatform(name: String): CompiledDefinition =
    CompiledDefinition(name, 0, body = _ => Left(UndefinedOnPlatform(name)))

  // this is responsible for QQ's function application semantics
  def standardEffectDistribution(func: Vector[JSON] => JSON => CompiledFilterResult[Vector[JSON]])
                                (args: Vector[CompiledFilter]): OrCompilationError[CompiledFilter] =
    Right(
      CompiledFilter.singleton {
        (j: JSON) =>
          for {
            rs <- args.traverseA[CompiledFilterStack, Vector[JSON]](_ (j))
            a <- func(rs.flatten)(j)
          } yield a
      }
    )

  final def noParamDefinition(name: String, fun: CompiledFilter): CompiledDefinition = {
    CompiledDefinition(
      name, numParams = 0, { _ => Right(fun) }
    )
  }
}

