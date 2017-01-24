package qq
package data

import cats.implicits._
import org.atnos.eff.Eff
import org.atnos.eff.syntax.all._
import qq.cc._

final case class CompiledDefinition[C]
(name: String, numParams: Int, body: (Vector[C] => OrCompilationError[C]))

object CompiledDefinition {
  def undefinedOnPlatform[C](name: String): CompiledDefinition[C] =
    CompiledDefinition[C](name, 0, body = _ => Left(UndefinedOnPlatform(name)))

  // this is responsible for QQ's function application semantics
  def standardEffectDistribution(func: Vector[JSON] => JSON => Eff[InterpretedFilterStack, Vector[JSON]])
                                (args: Vector[InterpretedFilter]): OrCompilationError[InterpretedFilter] =
    Right(
      InterpretedFilter.singleton {
        (j: JSON) =>
          for {
            rs <- args.traverseA[InterpretedFilterStack, Vector[JSON]](_ (j))
            a <- func(rs.flatten)(j)
          } yield a
      }
    )

  final def noParamDefinition[C](name: String, fun: C): CompiledDefinition[C] = {
    CompiledDefinition(
      name, numParams = 0, { _ => Right(fun) }
    )
  }
}

