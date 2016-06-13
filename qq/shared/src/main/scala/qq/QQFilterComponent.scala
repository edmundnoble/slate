package qq

import scalaz.{Applicative, Functor, Traverse, \/}
import scalaz.syntax.traverse._
import scalaz.std.list._

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

  sealed abstract class ConstantComponent[A] extends QQFilterComponent[A]
  final case class ConstInt[A](value: Double) extends ConstantComponent[A]
  final case class ConstString[A](value: String) extends ConstantComponent[A]

  final case class AddFilters[A](first: A, second: A) extends QQFilterComponent[A]
  final case class SubtractFilters[A](first: A, second: A) extends QQFilterComponent[A]
  final case class MultiplyFilters[A](first: A, second: A) extends QQFilterComponent[A]
  final case class DivideFilters[A](first: A, second: A) extends QQFilterComponent[A]
  final case class ModuloFilters[A](first: A, second: A) extends QQFilterComponent[A]

  implicit def qqfiltercomponent = new Traverse[QQFilterComponent] {

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

    override def traverseImpl[G[_], A, B](fa: QQFilterComponent[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[QQFilterComponent[B]] = {
      fa match {
        case IdFilter() => G.point(IdFilter())
        case FetchApi() => G.point(FetchApi())
        case CallFilter(name) => G.point(CallFilter(name))
        case SelectKey(k) => G.point(SelectKey(k))
        case SelectIndex(i) => G.point(SelectIndex(i))
        case SelectRange(s, e) => G.point(SelectRange(s, e))
        case ComposeFilters(first, second) => G.apply2(f(first), f(second))(ComposeFilters(_, _))
        case SilenceExceptions(a) => G.map(f(a))(SilenceExceptions(_))
        case EnlistFilter(a) => G.map(f(a))(EnlistFilter(_))
        case CollectResults(a) => G.map(f(a))(CollectResults(_))
        case EnsequenceFilters(as) => as.traverse(f).map(EnsequenceFilters(_))
        case EnjectFilters(obj) => obj.traverse { case (k, v) => G.tuple2(k.traverse(f), f(v)) }.map(EnjectFilters(_))
      }
    }
  }

}
