package edmin

import monix.reactive.Observable

import scala.scalajs.js

object QQAST {
    sealed trait QQFilter extends Any
    case object IdFilter extends QQFilter
    case object FetchApi extends QQFilter
    case class ComposeFilters(first: QQFilter, second: QQFilter) extends QQFilter
    case class SilenceExceptions(f: QQFilter) extends QQFilter
    case class EnlistFilter(f: QQFilter) extends QQFilter
    case class CollectResults(f: QQFilter) extends QQFilter
    case class EnsequenceFilters(filters: QQFilter*) extends QQFilter
    case class SelectKey(key: String) extends QQFilter
    case class SelectIndex(index: Int) extends QQFilter
    case class SelectRange(start: Int, end: Int) extends QQFilter

    type Optimization = PartialFunction[QQFilter, QQFilter]

    type CompiledFilter = js.Any => Observable[js.Any]

    class QQRuntimeException(message: String) extends RuntimeException(message)

    def idCompose: Optimization = {
      case ComposeFilters(IdFilter, s) => optimize(s)
      case ComposeFilters(f, IdFilter) => optimize(f)
    }

    def ensequenceSingle: Optimization = {
      case EnsequenceFilters(filter) => optimize(filter)
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
          case f: EnsequenceFilters => EnsequenceFilters(f.filters.map(optimize): _*)
          case f@SelectKey(_) => f
          case f@SelectIndex(_) => f
          case f@SelectRange(_, _) => f
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
      Observable.fromTask(
        filter(jsv).foldLeftF[List[js.Any]](Nil)((x, y) => y :: x).firstL
      ).map[js.Array[js.Any]](res => js.Array(res.getOrElse[List[js.Any]](Nil): _*))
    }

    def ensequenceCompiledFilters(functions: Seq[CompiledFilter]): CompiledFilter = { jsv: js.Any =>
      Observable.concat(functions.map(_ (jsv)): _*)
    }

    def selectKey(key: String): CompiledFilter = {
      case f: js.Object =>
        f.asInstanceOf[js.Dictionary[js.Object]].get(key) match {
          case None => Observable.pure(null)
          case Some(v) => Observable.pure(v)
        }
      case v =>
        Observable.raiseError(new QQRuntimeException(s"Tried to select key $key in $v but it's not a dictionary"))
    }

    def selectIndex(index: Int): CompiledFilter = {
      case f: js.Array[js.Object @unchecked] =>
        if (index >= -f.length) {
          if (index >= 0 && index < f.length) {
            Observable.pure(f(index))
          } else if (index < 0) {
            Observable.pure(f(f.length + index))
          } else {
            Observable.pure(null)
          }
        } else {
          Observable.pure(null)
        }
      case v =>
        Observable.raiseError(new QQRuntimeException(s"Tried to select index $index in $v but it's not an array"))
    }

    def selectRange(start: Int, end: Int): CompiledFilter = {
      case f: js.Array[js.Object @unchecked] =>
        if (start < end && start < f.length) {
          Observable.pure(f.jsSlice(start, end))
        } else {
          Observable.pure(js.Array[js.Object]())
        }
    }

    def collectResults(f: CompiledFilter): CompiledFilter = {
      case arr: js.Array[js.Object @unchecked] =>
        Observable.fromIterable(arr)
      case dict: js.Object =>
        Observable.fromIterable(dict.asInstanceOf[js.Dictionary[js.Object]].values)
      case v =>
        Observable.raiseError(new QQRuntimeException(s"Tried to flatten $v but it's not an array"))
    }

    def compile(filter: QQFilter): CompiledFilter = filter match {
      case IdFilter => jsv => Observable.pure(jsv)
      case ComposeFilters(f, s) => composeCompiledFilters(compile(f), compile(s))
      case EnlistFilter(f) => enlistCompiledFilters(compile(f))
      case SilenceExceptions(f) => jsv => compile(f)(jsv).onErrorRecoverWith {
        case _: QQRuntimeException => Observable.empty
      }
      case CollectResults(f) => collectResults(compile(f))
      case f: EnsequenceFilters => ensequenceCompiledFilters(f.filters.map(compile))
      case SelectKey(k) => selectKey(k)
      case SelectIndex(i) => selectIndex(i)
      case SelectRange(s, e) => selectRange(s, e)
    }

  }
