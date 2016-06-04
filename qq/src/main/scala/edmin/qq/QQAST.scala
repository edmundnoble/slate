package edmin.qq

import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import monocle.macros.Lenses
import monocle.macros._
import monocle.Lens

import scala.scalajs.js
import scalaz.{EitherT, \/, \/-}
import scalaz.syntax.traverse._
import scalaz.syntax.std.option._
import scalaz.syntax.id._
import scalaz.syntax.monad._
import scalaz.syntax.validation._
import scalaz.syntax.either._
import scalaz.std.list._
import Util._
import com.thoughtworks.each.Monadic._

import scala.scalajs.js.Array

object QQAST {
  sealed trait QQFilter extends Any
  case object IdFilter extends QQFilter
  case object FetchApi extends QQFilter
  case class ComposeFilters(first: QQFilter, second: QQFilter) extends QQFilter
  case class SilenceExceptions(f: QQFilter) extends QQFilter
  case class EnlistFilter(f: QQFilter) extends QQFilter
  case class CollectResults(f: QQFilter) extends QQFilter
  case class EnsequenceFilters(filters: List[QQFilter]) extends QQFilter
  case class SelectKey(key: String) extends QQFilter
  case class SelectIndex(index: Int) extends QQFilter
  case class SelectRange(start: Int, end: Int) extends QQFilter
  case class CallFilter(name: String) extends QQFilter
  case class Definition(name: String, params: List[String], body: QQFilter)
  object Definition {
    val body = GenLens[Definition](_.body)
  }
  case class CompiledDefinition(name: String, params: List[String], body: CompiledFilter)

  type Optimization = PartialFunction[QQFilter, Coeval[QQFilter]]

  type CompiledFilter = js.Any => Task[List[js.Any]]

  val taskOfListOfNull: Task[List[Null]] = Task.now(List(null))
  val emptyArray: Array[js.Any] = new js.Array[js.Any](0)

  class QQRuntimeException(message: String) extends RuntimeException(message)
  class QQCompilationException(message: String) extends RuntimeException(message)
  case class NoSuchMethod(name: String) extends QQCompilationException(message = s"No such method: $name")

  def idCompose: Optimization = {
    case ComposeFilters(IdFilter, s) => Coeval.defer(optimize(s))
    case ComposeFilters(f, IdFilter) => Coeval.defer(optimize(f))
  }

  def ensequenceSingle: Optimization = {
    case EnsequenceFilters(oneFilter :: Nil) => Coeval.defer(optimize(oneFilter))
  }

  def optimize(ast: QQFilter): Coeval[QQFilter] = {
    (ensequenceSingle orElse idCompose).lift(ast).getOrElse {
      ast match {
        case f@IdFilter => Coeval.now(f)
        case f@FetchApi => Coeval.now(f)
        case ComposeFilters(f, s) =>
          (Coeval.defer(optimize(f)) |@| Coeval.defer(optimize(s))) { ComposeFilters }
        case SilenceExceptions(f) => Coeval.defer(optimize(f).map(SilenceExceptions))
        case EnlistFilter(f) => Coeval.defer(optimize(f).map(EnlistFilter))
        case CollectResults(f) => Coeval.defer(optimize(f).map(CollectResults))
        case EnsequenceFilters(filters) => filters.traverse(f => Coeval.defer(optimize(f))).map(EnsequenceFilters)
        case f@SelectKey(_) => Coeval.now(f)
        case f@SelectIndex(_) => Coeval.now(f)
        case f@SelectRange(_, _) => Coeval.now(f)
        case f@CallFilter(_) => Coeval.now(f)
      }
    }
  }

  def composeCompiledFilters(firstFilter: CompiledFilter, secondFilter: CompiledFilter): CompiledFilter = { jsv: js.Any =>
    for {
      firstResult <- firstFilter(jsv)
      secondResult <- firstResult.traverse(secondFilter)
    } yield secondResult.flatten
  }

  def enlistCompiledFilters(filter: CompiledFilter): CompiledFilter = { jsv: js.Any =>
    for {
      results <- filter(jsv)
    } yield js.Array(results: _*) :: Nil
  }

  def ensequenceCompiledFilters(functions: List[CompiledFilter]): CompiledFilter = { jsv: js.Any =>
    functions traverse (_ (jsv)) map (_.flatten)
  }

  def selectKey(key: String): CompiledFilter = {
    (jsv: js.Any) => jsv match {
      case f: js.Object =>
        f.asInstanceOf[js.Dictionary[js.Object]].get(key) match {
          case None => taskOfListOfNull
          case Some(v) => Task.now(v :: Nil)
        }
      case v =>
        Task.raiseError(new QQRuntimeException(s"Tried to select key $key in $v but it's not a dictionary"))
    }
  }

  def selectIndex(index: Int): CompiledFilter = {
    case f: js.Array[js.Object@unchecked] =>
      if (index >= -f.length) {
        if (index >= 0 && index < f.length) {
          Task.now(f(index) :: Nil)
        } else if (index < 0) {
          Task.now(f(f.length + index) :: Nil)
        } else {
          taskOfListOfNull
        }
      } else {
        taskOfListOfNull
      }
    case v =>
      Task.raiseError(new QQRuntimeException(s"Tried to select index $index in $v but it's not an array"))
  }

  def selectRange(start: Int, end: Int): CompiledFilter = {
    case f: js.Array[js.Object@unchecked] =>
      if (start < end && start < f.length) {
        Task.now(f.jsSlice(start, end) :: Nil)
      } else {
        Task.now(emptyArray :: Nil)
      }
  }

  def collectResults(f: CompiledFilter): CompiledFilter = {
    case arr: js.Array[js.Any@unchecked] =>
      Task.now(arr.toList)
    case dict: js.Object =>
      Task.now(dict.asInstanceOf[js.Dictionary[js.Object]].values.toList)
    case v =>
      Task.raiseError(new QQRuntimeException(s"Tried to flatten $v but it's not an array"))
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
    filter match {
      case IdFilter =>
        EitherT(Coeval.now(((jsv: js.Any) => Task.now(List(jsv))).right[QQCompilationException]))
      case ComposeFilters(f, s) =>
        for {
          compiledF <- EitherT(Coeval.defer(compile(definitions, f).run))
          compiledS <- EitherT(Coeval.defer(compile(definitions, s).run))
        } yield composeCompiledFilters(compiledF, compiledS)
      case EnlistFilter(f) => EitherT(Coeval.defer(compile(definitions, f).run)).map(enlistCompiledFilters)
      case SilenceExceptions(f) =>
        for {
          fun <- EitherT(Coeval.defer(compile(definitions, f).run))
          r = (jsv: js.Any) => fun(jsv).onErrorRecover {
            case _: QQRuntimeException => Nil
          }
        } yield r
      case CollectResults(f) =>
        EitherT(Coeval.defer(compile(definitions, f).run)).map(collectResults)
      case f: EnsequenceFilters =>
        val compiledFilters: EitherT[Coeval, QQCompilationException, List[CompiledFilter]] =
          f.filters.traverse(f => EitherT(Coeval.defer(compile(definitions, f).run)))
        compiledFilters.map(ensequenceCompiledFilters)
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

}
