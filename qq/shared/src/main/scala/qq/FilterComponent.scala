package qq

import scala.language.higherKinds
import scalaz.{Applicative, Functor, Traverse, \/}
import scalaz.syntax.traverse._
import scalaz.std.list._

sealed abstract class FilterComponent[A]

object FilterComponent {

  // AST nodes with no child nodes
  sealed abstract class LeafComponent[A] extends FilterComponent[A] {
    @inline
    final def retag[B]: LeafComponent[B] = this.asInstanceOf[LeafComponent[B]]
  }

  final case class IdFilter[A]() extends LeafComponent[A]
  final case class FetchApi[A]() extends LeafComponent[A]
  final case class ComposeFilters[A](first: A, second: A) extends FilterComponent[A]
  final case class SilenceExceptions[A](f: A) extends FilterComponent[A]
  final case class EnlistFilter[A](f: A) extends FilterComponent[A]
  final case class CollectResults[A](f: A) extends FilterComponent[A]
  final case class EnsequenceFilters[A](first: A, second: A) extends FilterComponent[A]
  final case class EnjectFilters[A](obj: List[((String \/ A), A)]) extends FilterComponent[A]

  final case class CallFilter[A](name: String, params: List[A]) extends FilterComponent[A]

  final case class AddFilters[A](first: A, second: A) extends FilterComponent[A]
  final case class SubtractFilters[A](first: A, second: A) extends FilterComponent[A]
  final case class MultiplyFilters[A](first: A, second: A) extends FilterComponent[A]
  final case class DivideFilters[A](first: A, second: A) extends FilterComponent[A]
  final case class ModuloFilters[A](first: A, second: A) extends FilterComponent[A]

  final case class SelectKey[A](key: String) extends LeafComponent[A]
  final case class SelectIndex[A](index: Int) extends LeafComponent[A]
  final case class SelectRange[A](start: Int, end: Int) extends LeafComponent[A]

  sealed abstract class ConstantComponent[A] extends LeafComponent[A]
  final case class ConstNumber[A](value: Double) extends ConstantComponent[A]
  final case class ConstString[A](value: String) extends ConstantComponent[A]

  implicit def qqFilterComponentTraverse = new Traverse[FilterComponent] {

    override def map[A, B](fa: FilterComponent[A])(f: (A) => B): FilterComponent[B] = fa match {
      case i: IdFilter[_] => i.retag[B]
      case f: FetchApi[_] => f.retag[B]
      case CallFilter(name, params) => CallFilter(name, params map f)
      case k: SelectKey[_] => k.retag[B]
      case i: SelectIndex[_] => i.retag[B]
      case r: SelectRange[_] => r.retag[B]
      case n: ConstNumber[_] => n.retag[B]
      case s: ConstString[_] => s.retag[B]
      case AddFilters(first, second) => AddFilters(f(first), f(second))
      case SubtractFilters(first, second) => SubtractFilters(f(first), f(second))
      case MultiplyFilters(first, second) => MultiplyFilters(f(first), f(second))
      case DivideFilters(first, second) => DivideFilters(f(first), f(second))
      case ModuloFilters(first, second) => ModuloFilters(f(first), f(second))
      case ComposeFilters(first, second) => ComposeFilters(f(first), f(second))
      case SilenceExceptions(a) => SilenceExceptions(f(a))
      case EnlistFilter(a) => EnlistFilter(f(a))
      case CollectResults(a) => CollectResults(f(a))
      case EnsequenceFilters(first, second) => EnsequenceFilters(f(first), f(second))
      case EnjectFilters(obj) => EnjectFilters(obj.map { case (k, v) => k.map(f) -> f(v) })
    }

    override def traverseImpl[G[_], A, B](fa: FilterComponent[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[FilterComponent[B]] = {
      fa match {
        case i: IdFilter[_] => G.point(i.retag[B])
        case f: FetchApi[_] => G.point(f.retag[B])
        case CallFilter(name, params) => params.traverse(f).map(CallFilter(name, _))
        case k: SelectKey[_] => G.point(k.retag[B])
        case i: SelectIndex[_] => G.point(i.retag[B])
        case r: SelectRange[_] => G.point(r.retag[B])
        case n: ConstNumber[_] => G.point(n.retag[B])
        case s: ConstString[_] => G.point(s.retag[B])
        case AddFilters(first, second) => G.apply2(f(first), f(second))(AddFilters(_, _))
        case SubtractFilters(first, second) => G.apply2(f(first), f(second))(SubtractFilters(_, _))
        case MultiplyFilters(first, second) => G.apply2(f(first), f(second))(MultiplyFilters(_, _))
        case DivideFilters(first, second) => G.apply2(f(first), f(second))(DivideFilters(_, _))
        case ModuloFilters(first, second) => G.apply2(f(first), f(second))(ModuloFilters(_, _))
        case ComposeFilters(first, second) => G.apply2(f(first), f(second))(ComposeFilters(_, _))
        case SilenceExceptions(a) => G.map(f(a))(SilenceExceptions(_))
        case EnlistFilter(a) => G.map(f(a))(EnlistFilter(_))
        case CollectResults(a) => G.map(f(a))(CollectResults(_))
        case EnsequenceFilters(first, second) => G.apply2(f(first), f(second))(EnsequenceFilters(_, _))
        case EnjectFilters(obj) => obj.traverse { case (k, v) => G.tuple2(k.traverse(f), f(v)) }.map(EnjectFilters(_))
      }
    }
  }

}
