package qq

import monocle.macros._

import scala.language.higherKinds
import scalaz.{Functor, \/}
import matryoshka._
import FunctorT.ops._
import scalaz.syntax.arrow._
import scalaz.std.function._

object QQAST {

  sealed abstract class QQFilterComponent[A]
  object QQFilterComponent {
    final case class IdFilter[A]() extends QQFilterComponent[A]
    final case class FetchApi[A]() extends QQFilterComponent[A]
    final case class ComposeFilters[A](first: A, second: A) extends QQFilterComponent[A]
    final case class SilenceExceptions[A](f: A) extends QQFilterComponent[A]
    final case class EnlistFilter[A](f: A) extends QQFilterComponent[A]
    final case class CollectResults[A](f: A) extends QQFilterComponent[A]
    final case class EnsequenceFilters[A](filters: List[A]) extends QQFilterComponent[A]
    final case class EnjectFilters[A](obj: List[((String \/ A), A)]) extends QQFilterComponent[A]
    final case class CallFilter[A](name: String) extends QQFilterComponent[A]
    final case class SelectKey[A](key: String) extends QQFilterComponent[A]
    final case class SelectIndex[A](index: Int) extends QQFilterComponent[A]
    final case class SelectRange[A](start: Int, end: Int) extends QQFilterComponent[A]

    implicit def qqfiltercomponent = new Functor[QQFilterComponent] {
      override def map[A, B](fa: QQFilterComponent[A])(f: (A) => B): QQFilterComponent[B] = fa match {
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

  type QQProgram = Fix[QQFilterComponent]

  object QQProgram {

    import QQFilterComponent._

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

  final case class Definition(name: String, params: List[String], body: QQProgram)
  object Definition {
    val body = GenLens[Definition](_.body)
  }

}
