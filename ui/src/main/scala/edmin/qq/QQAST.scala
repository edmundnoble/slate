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
import scalaz.syntax.validation._
import scalaz.syntax.either._
import scalaz.std.list._
import Util._

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

  type Optimization = PartialFunction[QQFilter, QQFilter]

  type CompiledFilter = js.Any => Task[List[js.Any]]

  class QQRuntimeException(message: String) extends RuntimeException(message)
  class QQCompilationException(message: String) extends RuntimeException(message)
  case class NoSuchMethod(name: String) extends QQCompilationException(message = s"No such method: $name")

  def idCompose: Optimization = {
    case ComposeFilters(IdFilter, s) => optimize(s)
    case ComposeFilters(f, IdFilter) => optimize(f)
  }

  def ensequenceSingle: Optimization = {
    case EnsequenceFilters(oneFilter :: Nil) => optimize(oneFilter)
  }

  def optimize(ast: QQFilter): QQFilter = {
    (ensequenceSingle orElse idCompose).lift(ast).getOrElse {
      ast match {
        case f@IdFilter => f
        case f@FetchApi => f
        case ComposeFilters(f, s) => ComposeFilters(optimize(f), optimize(s))
        case SilenceExceptions(f) => SilenceExceptions(optimize(f))
        case EnlistFilter(f) => EnlistFilter(optimize(f))
        case CollectResults(f) => CollectResults(optimize(f))
        case f: EnsequenceFilters => EnsequenceFilters(f.filters.map(optimize))
        case f@SelectKey(_) => f
        case f@SelectIndex(_) => f
        case f@SelectRange(_, _) => f
        case f@CallFilter(_) => f
      }
    }
  }

  def composeCompiledFilters(firstFilter: CompiledFilter, secondFilter: CompiledFilter): CompiledFilter = { jsv: js.Any =>
    for {
      firstResult <- firstFilter(jsv)
      secondResult <- secondFilter(firstResult)
    } yield secondResult
  }

  def enlistCompiledFilters(filter: CompiledFilter): CompiledFilter = { jsv: js.Any =>
    for {
      results <- filter(jsv)
    } yield List(js.Array(results: _*))
  }

  def ensequenceCompiledFilters(functions: List[CompiledFilter]): CompiledFilter = { jsv: js.Any =>
    functions traverse (_ (jsv)) map (_.flatten)
  }

  def selectKey(key: String): CompiledFilter = {
    (jsv: js.Any) => jsv match {
      case f: js.Object =>
        f.asInstanceOf[js.Dictionary[js.Object]].get(key) match {
          case None => Task.now(List(null))
          case Some(v) => Task.now(List(v))
        }
      case v =>
        Task.raiseError(new QQRuntimeException(s"Tried to select key $key in $v but it's not a dictionary"))
    }
  }

  def selectIndex(index: Int): CompiledFilter = {
    case f: js.Array[js.Object@unchecked] =>
      if (index >= -f.length) {
        if (index >= 0 && index < f.length) {
          Task.now(List(f(index)))
        } else if (index < 0) {
          Task.now(List(f(f.length + index)))
        } else {
          Task.now(List(null))
        }
      } else {
        Task.now(List(null))
      }
    case v =>
      Task.raiseError(new QQRuntimeException(s"Tried to select index $index in $v but it's not an array"))
  }

  def selectRange(start: Int, end: Int): CompiledFilter = {
    case f: js.Array[js.Object@unchecked] =>
      if (start < end && start < f.length) {
        Task.now(List(f.jsSlice(start, end)))
      } else {
        Task.now(List(js.Array[js.Object]()))
      }
  }

  def collectResults(f: CompiledFilter): CompiledFilter = {
    case arr: js.Array[js.Object@unchecked] =>
      Task.now(arr.toList)
    case dict: js.Object =>
      Task.now(dict.asInstanceOf[js.Dictionary[js.Object]].values.toList)
    case v =>
      Task.raiseError(new QQRuntimeException(s"Tried to flatten $v but it's not an array"))
  }

  def compileProgram(definitions: List[Definition], main: QQFilter): QQCompilationException \/ CompiledFilter = {
    val compiledDefinitions: EitherT[Coeval, QQCompilationException, List[CompiledDefinition]] =
      definitions.foldLeft(EitherT.right[Coeval, QQCompilationException, List[CompiledDefinition]](Coeval.now(Nil))) {
        (soFar: EitherT[Coeval, QQCompilationException, List[CompiledDefinition]], next: Definition) =>
          for {
            definitionsSoFar <- soFar
            nextCompiledBody <- EitherT(Coeval.defer(compile(definitionsSoFar, next.body).run))
          } yield CompiledDefinition(next.name, next.params, nextCompiledBody) :: definitionsSoFar
      }
    val compileMain = for {
      definitions <- compiledDefinitions
      compiledMain <- compile(definitions, main)
    } yield compiledMain
    compileMain.run.runAttempt.value
  }

  def compile(definitions: List[CompiledDefinition], filter: QQFilter): EitherT[Coeval, QQCompilationException, CompiledFilter] = {
    filter match {
      case IdFilter =>
        EitherT.right[Coeval, QQCompilationException, CompiledFilter](Coeval.now((jsv: js.Any) => Task.now(List(jsv))))
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
        for {
          fun <- EitherT(Coeval.defer(compile(definitions, f).run))
        } yield collectResults(fun)
      case f: EnsequenceFilters =>
        val compiledFilters: EitherT[Coeval, QQCompilationException, List[CompiledFilter]] =
          f.filters.traverseU(f => EitherT(Coeval.defer(compile(definitions, f).run)))
        compiledFilters.map(ensequenceCompiledFilters)
      case SelectKey(k) =>
        EitherT.right[Coeval, QQCompilationException, CompiledFilter](Coeval.now(selectKey(k)))
      case SelectIndex(i) =>
        EitherT.right[Coeval, QQCompilationException, CompiledFilter](Coeval.now(selectIndex(i)))
      case SelectRange(s, e) =>
        EitherT.right[Coeval, QQCompilationException, CompiledFilter](Coeval.now(selectRange(s, e)))
      case CallFilter(filterIdentifier) =>
        EitherT.fromDisjunction[Coeval](
          definitions.find(_.name == filterIdentifier).toRightDisjunction[QQCompilationException](NoSuchMethod(filterIdentifier)).map(_.body)
        )
    }
  }

}
