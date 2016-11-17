package qq
package data


import cats.implicits._
import cats.{Applicative, Eval, Traverse}
import qq.util.Fix

import scala.language.higherKinds

// A single node of the QQ AST. Type parameter is used for child nodes.
sealed abstract class FilterComponent[A]

// AST nodes with no child nodes. Phantom-variant functors.
sealed abstract class LeafComponent[A] extends FilterComponent[A] {
  @inline
  final def retag[B]: LeafComponent[B] = this.asInstanceOf[LeafComponent[B]]
}

// AST nodes that represent filters ignoring their input.
sealed abstract class ConstantComponent[A] extends LeafComponent[A]

// AST node containing a path selector subtree and an operation
final case class PathOperation[A](pathComponents: List[PathComponent], operation: PathOperationF[A]) extends FilterComponent[A]

// Variable binding, with raw names and lexical scoping
final case class AsBinding[A](name: String, as: A, in: A) extends FilterComponent[A]

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

// Runs two filters at once, appending their result lists.
// Associative.
final case class EnsequenceFilters[A](first: A, second: A) extends FilterComponent[A]

// Creates a JSON object from some (string or filter, filter) pairs.
final case class EnjectFilters[A](obj: List[((String Either A), A)]) extends FilterComponent[A]

// Calls another filter or filter operator.
final case class CallFilter[A](name: String, params: List[A]) extends FilterComponent[A]

// not, i.e., bang (!)
final case class FilterNot[A]() extends LeafComponent[A]

// constants!
final case class ConstNumber[A](value: Double) extends ConstantComponent[A]

final case class ConstBoolean[A](value: Boolean) extends ConstantComponent[A]

final case class ConstString[A](value: String) extends ConstantComponent[A]

// Math, with operator as its own type
final case class FilterMath[A](first: A, second: A, op: MathOperator) extends FilterComponent[A]

sealed trait MathOperator

case object Add extends MathOperator

case object Subtract extends MathOperator

case object Multiply extends MathOperator

case object Divide extends MathOperator

case object Modulo extends MathOperator

case object Equal extends MathOperator

case object LTE extends MathOperator

case object GTE extends MathOperator

case object LessThan extends MathOperator

case object GreaterThan extends MathOperator

object FilterComponent {

  implicit def qqFilterComponentTraverse = new Traverse[FilterComponent] {

    override def traverse[G[_], A, B](fa: FilterComponent[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[FilterComponent[B]] = {
      fa match {
        case l: LeafComponent[_] => G.pure(l.retag[B])
        case PathOperation(components, operationF) => operationF.traverse(f).map(PathOperation(components, _))
        case AsBinding(name, as, in) => G.map2(f(as), f(in))(AsBinding(name, _, _))
        case CallFilter(name, params) => params.traverse(f).map(CallFilter(name, _))
        case FilterMath(first, second, op) => G.map2(f(first), f(second))(FilterMath(_, _, op))
        case ComposeFilters(first, second) => G.map2(f(first), f(second))(ComposeFilters(_, _))
        case SilenceExceptions(a) => G.map(f(a))(SilenceExceptions(_))
        case EnlistFilter(a) => G.map(f(a))(EnlistFilter(_))
        case EnsequenceFilters(first, second) => G.map2(f(first), f(second))(EnsequenceFilters(_, _))
        case EnjectFilters(obj) => obj.traverse[G, (String Either B, B)] { case (k, v) => G.tuple2(k.traverse(f), f(v)) }.map(EnjectFilters(_))
      }
    }
    // TODO
    override def foldLeft[A, B](fa: FilterComponent[A], b: B)(f: (B, A) => B): B =
      ???
    override def foldRight[A, B](fa: FilterComponent[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      ???
  }

  @inline final def embed
  (v: FilterComponent[FilterAST]): FilterAST = Fix(v)
}

