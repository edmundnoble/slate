package qq

import matryoshka.Corecursive

import scala.language.higherKinds
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.{Applicative, Traverse, \/}

// A single node of the QQ AST. Type parameter is used for child nodes.
sealed abstract class FilterComponent[A]

// AST nodes with no child nodes. Phantom-variant functors.
sealed abstract class LeafComponent[A] extends FilterComponent[A] {
  @inline
  final def retag[B]: LeafComponent[B] = this.asInstanceOf[LeafComponent[B]]
}

// Identity filter; function taking a value and returning it
final case class IdFilter[A]() extends LeafComponent[A]

// Let-binding, with raw names
final case class LetAsBinding[A](name: String, as: A, in: A) extends FilterComponent[A]

// Access let-bindings
final case class Dereference[A](name: String) extends ConstantComponent[A]

// Compose filters in "pipe" order
// ComposeFilters(first, second) is a filter which executes the parameter filters in named order on the input values.
// Associative.
final case class ComposeFilters[A](first: A, second: A) extends FilterComponent[A]

// Makes a new filter silence the exceptions coming from another filter
// Idempotent.
final case class SilenceExceptions[A](f: A) extends FilterComponent[A]

// Enlisting a filter entails taking the list of results it returns,
// and returning that as a single result in a JSON array.
// Inverse of CollectResults.
final case class EnlistFilter[A](f: A) extends FilterComponent[A]

// Collecting the results from a filter which returns JSON arrays
// yields a filter which concatenates the arrays' values into a single list of output values
// Inverse of EnlistFilter.
final case class CollectResults[A](f: A) extends FilterComponent[A]

// Runs two filters at once, appending their result lists.
// Associative.
final case class EnsequenceFilters[A](first: A, second: A) extends FilterComponent[A]

// Creates a JSON object from some (string \/ filter, filter) pairs.
final case class EnjectFilters[A](obj: List[((String \/ A), A)]) extends FilterComponent[A]

// Calls another filter or filter operator.
final case class CallFilter[A](name: String, params: List[A]) extends FilterComponent[A]

// Math, lots of JS-ish special cases for certain types.
final case class AddFilters[A](first: A, second: A) extends FilterComponent[A]

final case class SubtractFilters[A](first: A, second: A) extends FilterComponent[A]

final case class MultiplyFilters[A](first: A, second: A) extends FilterComponent[A]

final case class DivideFilters[A](first: A, second: A) extends FilterComponent[A]

final case class ModuloFilters[A](first: A, second: A) extends FilterComponent[A]

// Select key, index or range in JSON object or array.
// Return null if asked for something not contained in the target.
final case class SelectKey[A](key: String) extends LeafComponent[A]

final case class SelectIndex[A](index: Int) extends LeafComponent[A]

final case class SelectRange[A](start: Int, end: Int) extends LeafComponent[A]

// AST nodes that represent filters ignoring their input.
sealed abstract class ConstantComponent[A] extends LeafComponent[A]

final case class ConstNumber[A](value: Double) extends ConstantComponent[A]

final case class ConstString[A](value: String) extends ConstantComponent[A]

object FilterComponent {

  implicit def qqFilterComponentTraverse = new Traverse[FilterComponent] {

    override def traverseImpl[G[_], A, B](fa: FilterComponent[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[FilterComponent[B]] = {
      fa match {
        case l: LeafComponent[_] => G.point(l.retag[B])
        case LetAsBinding(name, as, in) => G.apply2(f(as), f(in))(LetAsBinding(name, _, _))
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

  // Partially apply FilterComponent parameter of C.embed
  @inline final def embed[T[_[_]]]
  (v: FilterComponent[T[FilterComponent]])(implicit C: Corecursive[T]): T[FilterComponent] = C.embed[FilterComponent](v)
}

