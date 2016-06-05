package edmin.qq

import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import monocle.macros.Lenses
import monocle.macros._
import monocle.Lens

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

import scala.language.higherKinds

object QQAST {
  sealed trait QQFilter
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

  type Optimization = PartialFunction[QQFilter, Coeval[QQFilter]]

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

}
