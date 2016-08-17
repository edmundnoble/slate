package qq

import scala.language.higherKinds
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.{Applicative, Traverse, \/}

sealed abstract class FilterComponent[A]

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

// AST nodes that represent filters ignoring their input
sealed abstract class ConstantComponent[A] extends LeafComponent[A]
final case class ConstNumber[A](value: Double) extends ConstantComponent[A]
final case class ConstString[A](value: String) extends ConstantComponent[A]

object FilterComponent {

  implicit def qqFilterComponentTraverse = new Traverse[FilterComponent] {

    override def traverseImpl[G[_], A, B](fa: FilterComponent[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[FilterComponent[B]] = {
      fa match {
        case l: LeafComponent[_] => G.point(l.retag[B])
        case CallFilter(name, params) => params.traverse(f).map(CallFilter(name, _))
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
        case EnjectFilters(obj) => obj.traverse[G, (String \/ B, B)] { case (k, v) => G.tuple2(k.traverse(f), f(v)) }.map(EnjectFilters(_))
      }
    }
  }

}
