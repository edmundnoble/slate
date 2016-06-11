package qq

import qq.Util._
import qq.QQCompiler.{NoSuchMethod, QQCompilationException, QQRuntimeException}
import qq.Definition
import qq.QQFilterComponent._

import monix.eval.{Coeval, Task}
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scalaz.{EitherT, \/}
import matryoshka._
import FunctorT.ops._

abstract class QQCompiler {

  type AnyTy
  type CompiledFilter = AnyTy => Task[List[AnyTy]]

  case class CompiledDefinition(name: String, params: List[String], body: CompiledFilter)

  def composeCompiledFilters(firstFilter: CompiledFilter, secondFilter: CompiledFilter): CompiledFilter = { jsv: AnyTy =>
    for {
      firstResult <- firstFilter(jsv)
      secondResult <- firstResult.traverse(secondFilter)
    } yield secondResult.flatten
  }

  def ensequenceCompiledFilters(functions: List[CompiledFilter]): CompiledFilter = { jsv: AnyTy =>
    Task.sequence(functions.map(_ (jsv))).map(_.flatten)
  }

  def compileProgram(definitions: List[Definition], main: QQFilter): QQCompilationException \/ CompiledFilter = {
    val compiledDefinitions: EitherT[Coeval, QQCompilationException, List[CompiledDefinition]] =
      definitions.foldLeft(EitherT(Coeval.now(nil[CompiledDefinition].right[QQCompilationException]))) {
        (soFar, nextDefinition) =>
          soFar.flatMap(definitionsSoFar =>
            EitherT(Coeval.defer(compile(definitionsSoFar, nextDefinition.body).run))
              .map(CompiledDefinition(nextDefinition.name, nextDefinition.params, _) :: definitionsSoFar))
      }
    compiledDefinitions.flatMap(compile(_, main)).run.runAttempt.value
  }

  def compile(definitions: List[CompiledDefinition], filter: QQFilter): EitherT[Coeval, QQCompilationException, CompiledFilter] = {
    filter.unFix match {
      case IdFilter() =>
        EitherT(Coeval.now(((jsv: AnyTy) => Task.now(List(jsv))).right[QQCompilationException]))
      case ComposeFilters(f, s) =>
        for {
          compiledF <- EitherT(Coeval.defer(compile(definitions, f).run))
          compiledS <- EitherT(Coeval.defer(compile(definitions, s).run))
        } yield composeCompiledFilters(compiledF, compiledS)
      case EnlistFilter(f) => EitherT(Coeval.defer(compile(definitions, f).run)).map(enlistFilter)
      case SilenceExceptions(f) =>
        for {
          fun <- EitherT(Coeval.defer(compile(definitions, f).run))
          r = (jsv: AnyTy) => fun(jsv).onErrorRecover {
            case _: QQRuntimeException => Nil
          }
        } yield r
      case CollectResults(f) =>
        EitherT(Coeval.defer(compile(definitions, f).run)).map(collectResults)
      case EnsequenceFilters(filters) =>
        val compiledFilters: EitherT[Coeval, QQCompilationException, List[CompiledFilter]] =
          filters.traverse(f => EitherT(Coeval.defer(compile(definitions, f).run)))
        compiledFilters.map(ensequenceCompiledFilters)
      case EnjectFilters(obj) =>
        val compiledDown: EitherT[Coeval, QQCompilationException, List[(String \/ CompiledFilter, CompiledFilter)]] = obj.traverse {
          case (k, v) => {
            k.traverse(compile(definitions, _)).flatMap(e => compile(definitions, v).map(e -> _))
          }
        }
        compiledDown.map(enjectFilter)
      case SelectKey(k) =>
        EitherT(Coeval.now(selectKey(k).right[QQCompilationException]))
      case SelectIndex(i) =>
        EitherT(Coeval.now(selectIndex(i).right[QQCompilationException]))
      case SelectRange(s, e) =>
        EitherT(Coeval.now(selectRange(s, e).right[QQCompilationException]))
      case CallFilter(filterIdentifier) =>
        EitherT(Coeval.now(
          definitions
            .find(_.name == filterIdentifier)
            .cata(_.body.right, NoSuchMethod(filterIdentifier).left)
        ))
    }
  }

  def enjectFilter(obj: List[(\/[String, CompiledFilter], CompiledFilter)]): CompiledFilter
  def enlistFilter(filter: CompiledFilter): CompiledFilter
  def selectKey(key: String): CompiledFilter
  def selectIndex(index: Int): CompiledFilter
  def selectRange(start: Int, end: Int): CompiledFilter
  def collectResults(f: CompiledFilter): CompiledFilter
}

object QQCompiler {

  class QQRuntimeException(message: String) extends RuntimeException(message)
  class QQCompilationException(message: String) extends RuntimeException(message)
  case class NoSuchMethod(name: String) extends QQCompilationException(message = s"No such method: $name")

}
