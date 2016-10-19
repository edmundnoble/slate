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
  def standardEffectDistribution(func: List[JSON] => JSON => Task[ValidatedNel[QQRuntimeError, JSON]])(args: List[CompiledFilter]): OrCompilationError[CompiledFilter] = {
    (b: VarBindings) =>
      (j: JSON) =>
        val map = args.map(_ (b)(j))
        val gather: Task[List[Validated[NonEmptyList[QQRuntimeError], List[JSON]]]] = Task.gather(map)
        //        def standardEffectDistribution(func: List[JSON] => JSON => Task[JSON])(args: List[CompiledFilter]): OrCompilationError[CompiledFilter] =
        //          ((b: VarBindings) => (j: JSON) =>
        //            Task.gather(args.map(_ (b)(j))).flatMap(llj => Task.gather(util.foldWithPrefixes(llj.head, llj.tail: _*).map(l => func(l.reverse)(j))))).right
        val x: Task[ValidatedNel[QQRuntimeError, List[List[JSON]]]] =
        gather.map(llj => llj.traverse[ValidatedNel[QQRuntimeError, ?], List[JSON]](identity))
        val x2: Task[ValidatedNel[QQRuntimeError, ValidatedNel[QQRuntimeError, List[JSON]]]] =
          x.flatMap { a =>
            val y: Task[Validated[NonEmptyList[QQRuntimeError], ValidatedNel[QQRuntimeError, List[JSON]]]] =
              a.traverse[Task, NonEmptyList[QQRuntimeError], ValidatedNel[QQRuntimeError, List[JSON]]] { llj =>
              val r: Task[List[Validated[NonEmptyList[QQRuntimeError], JSON]]] =
                Task.gather(util.foldWithPrefixes(llj.head, llj.tail: _*).map(l => func(l.reverse)(j)))
              val r2: Task[ValidatedNel[QQRuntimeError, List[JSON]]] =
                r.map(_.traverse[ValidatedNel[QQRuntimeError, ?], JSON](identity))
              r2
            }
            y
          }
        val x3: Task[ValidatedNel[QQRuntimeError, List[JSON]]] =
          x2.map(_.flatten)

        x3
    //        Task.gather(util.foldWithPrefixes(llj.head, llj.tail: _*).map(l => func(l.reverse)(j))))
  }.right

  final def noParamDefinition(name: String, fun: CompiledFilter): CompiledDefinition = {
    CompiledDefinition(
      name, numParams = 0, { case Nil => fun.right[QQCompilationException] }
    )
  }
}

