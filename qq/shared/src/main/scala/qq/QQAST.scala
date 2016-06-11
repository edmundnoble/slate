package qq

import qq.Util._
import monix.eval.Coeval
import monocle.macros._

import scala.language.higherKinds
import scalaz.std.list._
import scalaz.{Functor, \/, ~>}
import scalaz.syntax.monad._
import scalaz.syntax.traverse._
import matryoshka._
import FunctorT.ops._
import scalaz.syntax.arrow._
import scalaz.std.function._

object QQAST {
  type QQProgram = Fix[QQFilter]
  object QQProgram {
    def id: QQProgram = Fix(IdFilter())
    def fetch: QQProgram = Fix(FetchApi())
    def compose(first: QQProgram, second: QQProgram): QQProgram = Fix(ComposeFilters(first, second))
    def silence(f: QQProgram): QQProgram = Fix(SilenceExceptions(f))
    def enlist(f: QQProgram): QQProgram = Fix(EnlistFilter(f))
    def collectResults(f: QQProgram): QQProgram = Fix(CollectResults(f))
    def ensequence(filters: List[QQProgram]): QQProgram = Fix(EnsequenceFilters(filters))
    def enject(obj: List[((String \/ QQProgram), QQProgram)]): QQProgram = Fix(EnjectFilters(obj))
    def call(name: String): QQProgram = Fix(CallFilter(name))
    def selectKey(key: String): QQProgram = Fix(SelectKey(key))
    def selectIndex(index: Int): QQProgram = Fix(SelectIndex(index))
    def selectRange(start: Int, end: Int): QQProgram = Fix(SelectRange(start, end))
  }

  sealed trait QQFilter[A]
  final case class IdFilter[A] private () extends QQFilter[A]
  final case class FetchApi[A] private () extends QQFilter[A]
  final case class ComposeFilters[A] private (first: A, second: A) extends QQFilter[A]
  final case class SilenceExceptions[A] private (f: A) extends QQFilter[A]
  final case class EnlistFilter[A] private (f: A) extends QQFilter[A]
  final case class CollectResults[A] private (f: A) extends QQFilter[A]
  final case class EnsequenceFilters[A] private (filters: List[A]) extends QQFilter[A]
  final case class EnjectFilters[A] private (obj: List[((String \/ A), A)]) extends QQFilter[A]
  final case class CallFilter[A] private (name: String) extends QQFilter[A]
  final case class SelectKey[A] private (key: String) extends QQFilter[A]
  final case class SelectIndex[A] private (index: Int) extends QQFilter[A]
  final case class SelectRange[A] private (start: Int, end: Int) extends QQFilter[A]

  object QQFilter {
    implicit def qqfunctor = new Functor[QQFilter] {
      override def map[A, B](fa: QQFilter[A])(f: (A) => B): QQFilter[B] = fa match {
        case IdFilter() => IdFilter()
        case FetchApi() => FetchApi()
        case CallFilter(name) => CallFilter(name)
        case SelectKey(k) => SelectKey(k)
        case SelectIndex(i) => SelectIndex(i)
        case SelectRange(s, e) => SelectRange(s, e)
        case ComposeFilters(first, second) => ComposeFilters(f(first), f(second))
        case SilenceExceptions(a) => SilenceExceptions(f(a))
        case EnlistFilter(a) => EnlistFilter(f(a))
        case CollectResults(a) => CollectResults(f(a))
        case EnsequenceFilters(as) => EnsequenceFilters(as.map(f))
        case EnjectFilters(obj) => EnjectFilters(obj.map { case (k, v) => k.map(f) -> f(v) })
      }
    }
  }

  final case class Definition(name: String, params: List[String], body: QQProgram)
  object Definition {
    val body = GenLens[Definition](_.body)
  }

  type Optimization = Fix[QQFilter] => Fix[QQFilter]

  def idCompose: Optimization = {
    case Fix(ComposeFilters(Fix(IdFilter()), s)) => s
    case Fix(ComposeFilters(f, Fix(IdFilter()))) => f
    case x => x
  }

  def ensequenceSingle: Optimization = {
    case Fix(EnsequenceFilters(onef :: Nil)) => onef
    case x => x
  }

  val optimizations = Vector(idCompose, ensequenceSingle)
  val optimize = optimizations.reduceLeft(_ <<< _)

}
