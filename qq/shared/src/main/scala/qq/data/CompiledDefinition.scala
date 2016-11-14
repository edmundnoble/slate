package qq
package data

import cats.data.{ValidatedNel, Validated, NonEmptyList}
import monix.eval.Task
import util._
import cc._
import monix.cats._
import cats.implicits._
import org.atnos.eff._, Eff._, syntax.all._

final case class CompiledDefinition
(name: String, numParams: Int, body: (List[CompiledFilter] => OrCompilationError[CompiledFilter]))

object CompiledDefinition {
  def undefinedOnPlatform(name: String): CompiledDefinition =
    CompiledDefinition(name, 0, body = _ => UndefinedOnPlatform(name).left)

  // this is responsible for QQ's function application semantics
  def standardEffectDistribution(func: List[JSON] => JSON => CompiledFilterResult[List[JSON]])
                                (args: List[CompiledFilter]): OrCompilationError[CompiledFilter] = CompiledFilter.singleton {
    (j: JSON) =>
      for {
        rs <- args.traverseA[CompiledFilterStack, List[JSON]](_ (j))
        a <- func(rs.flatten)(j)
      } yield a
  }.right

  final def noParamDefinition(name: String, fun: CompiledFilter): CompiledDefinition = {
    CompiledDefinition(
      name, numParams = 0, { _ => fun.right[QQCompilationException] }
    )
  }
}

