package qq
package data

import monix.eval.Task
import cc._

import scalaz.syntax.either._

final case class CompiledDefinition
(name: String, numParams: Int, body: (List[CompiledFilter] => OrCompilationError[CompiledFilter]))

object CompiledDefinition {
  def undefinedOnPlatform(name: String): CompiledDefinition =
    CompiledDefinition(name, 0, body = _ => UndefinedOnPlatform(name).left)

  // this is responsible for QQ's function application semantics
  def standardEffectDistribution(func: List[JSON] => JSON => Task[JSON])(args: List[CompiledFilter]): OrCompilationError[CompiledFilter] =
    ((b: VarBindings) => (j: JSON) =>
      Task.gather(args.map(_ (b)(j))).flatMap(llj => Task.gather(util.foldWithPrefixes(llj.head, llj.tail: _*).map(l => func(l.reverse)(j))))).right

  final def noParamDefinition(name: String, fun: CompiledFilter): CompiledDefinition = {
    CompiledDefinition(
      name, numParams = 0, { case Nil => fun.right[QQCompilationException] }
    )
  }
}

