package qq
package data

import monix.eval.Task
import util._
import cc._

import monix.scalaz._
import scalaz.{NonEmptyList, Validation, ValidationNel}
import scalaz.syntax.either._
import scalaz.syntax.traverse._
import scalaz.std.list._

final case class CompiledDefinition
(name: String, numParams: Int, body: (List[CompiledFilter] => OrCompilationError[CompiledFilter]))

object CompiledDefinition {
  def undefinedOnPlatform(name: String): CompiledDefinition =
    CompiledDefinition(name, 0, body = _ => UndefinedOnPlatform(name).left)

  // this is responsible for QQ's function application semantics
  def standardEffectDistribution(func: List[JSON] => JSON => Task[ValidationNel[QQRuntimeError, JSON]])(args: List[CompiledFilter]): OrCompilationError[CompiledFilter] = {
    (b: VarBindings) =>
      (j: JSON) =>
        import Validation.FlatMap._
        val map = args.map(_ (b)(j))
        val gather: Task[List[Validation[NonEmptyList[QQRuntimeError], List[JSON]]]] = Task.gather(map)
        //        def standardEffectDistribution(func: List[JSON] => JSON => Task[JSON])(args: List[CompiledFilter]): OrCompilationError[CompiledFilter] =
        //          ((b: VarBindings) => (j: JSON) =>
        //            Task.gather(args.map(_ (b)(j))).flatMap(llj => Task.gather(util.foldWithPrefixes(llj.head, llj.tail: _*).map(l => func(l.reverse)(j))))).right
        val x: Task[ValidationNel[QQRuntimeError, List[List[JSON]]]] =
        gather.map(llj => llj.traverse[ValidationNel[QQRuntimeError, ?], List[JSON]](identity))
        val x2: Task[ValidationNel[QQRuntimeError, ValidationNel[QQRuntimeError, List[JSON]]]] =
          x.flatMap { a =>
            val y: Task[Validation[NonEmptyList[QQRuntimeError], ValidationNel[QQRuntimeError, List[JSON]]]] =
              a.traverse[Task, NonEmptyList[QQRuntimeError], ValidationNel[QQRuntimeError, List[JSON]]] { llj =>
              val r: Task[List[Validation[NonEmptyList[QQRuntimeError], JSON]]] =
                Task.gather(util.foldWithPrefixes(llj.head, llj.tail: _*).map(l => func(l.reverse)(j)))
              val r2: Task[ValidationNel[QQRuntimeError, List[JSON]]] =
                r.map(_.traverse[ValidationNel[QQRuntimeError, ?], JSON](identity))
              r2
            }
            y
          }
        val x3: Task[ValidationNel[QQRuntimeError, List[JSON]]] =
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

