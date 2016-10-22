package qq
package data

import cats.data.{ValidatedNel, Validated, NonEmptyList}
import monix.eval.Task
import util._
import cc._
import monix.cats._
import cats.implicits._

final case class CompiledDefinition
(name: String, numParams: Int, body: (List[CompiledFilter] => OrCompilationError[CompiledFilter]))

object CompiledDefinition {
  def undefinedOnPlatform(name: String): CompiledDefinition =
    CompiledDefinition(name, 0, body = _ => UndefinedOnPlatform(name).left)

  // this is responsible for QQ's function application semantics
  def standardEffectDistribution(func: List[JSON] => JSON => CompiledFilterResult[JSON])
                                (args: List[CompiledFilter]): OrCompilationError[CompiledFilter] = CompiledFilter.singleton {
    (j: JSON) =>
      for {
        rs <- args.traverse[CompiledFilterResult, JSON](_ (j))
        a <- func(rs)(j)
      } yield a
  }.right

  final def noParamDefinition(name: String, fun: CompiledFilter): CompiledDefinition = {
    CompiledDefinition(
      name, numParams = 0, { case Nil => fun.right[QQCompilationException] }
    )
  }
}

