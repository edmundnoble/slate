package qq
package data

import monix.eval.Task
import qq.cc._

import scalaz.syntax.either._

final case class CompiledDefinition[J]
(name: String, numParams: Int, body: (List[CompiledFilter[J]] => OrCompilationError[CompiledFilter[J]]))

object CompiledDefinition {
  def undefinedOnPlatform[J](name: String): CompiledDefinition[J] =
    CompiledDefinition[J](name, 0, body = _ => UndefinedOnPlatform(name).left)

  // this is responsible for QQ's function application semantics
  def standardEffectDistribution[J](func: List[J] => Task[J]): List[CompiledFilter[J]] => OrCompilationError[CompiledFilter[J]] =
    args =>
      ((b: VarBindings[J]) => (j: J) =>
        Task.gather(args.map(_ (b)(j))).flatMap(llj => Task.gather(util.foldWithPrefixes(llj.head, llj.tail: _*).map(l => func(l.reverse))))).right

  final def noParamDefinition[J](name: String, fun: CompiledFilter[J]): CompiledDefinition[J] = {
    CompiledDefinition[J](
      name,
      numParams = 0, { case Nil => fun.right[QQCompilationException] }
    )
  }
}

