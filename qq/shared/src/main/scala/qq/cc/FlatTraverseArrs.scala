package qq
package cc

import cats.data.NonEmptyVector
import cats.implicits._
import cats.{Monad, Traverse}
import monix.eval.Task
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import qq.util._

/*
This is an experimental construct I created to address stack-safety problems with composing filters in QQ.
It essentially represents a tree of TraverseM arrows (I => Eff[R, T[O]]) which need to be composed.
It is VERY similar to the Arrs construct in Eff.
A FlatTraverseArrs[R, T, I, O] is a function I => Eff[R, T[O]] with re-associated, trampolined composition.
The trampolining is performed inside the result effect.
 */
sealed trait FlatTraverseArrs[R, T[_], I, O] extends Any {
  def apply(i: I)(implicit ev1: Traverse[T], ev2: Monad[T], ev3: Member[TaskParallel, R]): Eff[R, T[O]] =
    suspend(FlatTraverseArrs.run[R, T, I, O](this)(i))
}
case class Encompose[R, T[_], I, O](arrs: NonEmptyVector[FlatTraverseArrs[R, T, Any, Any]]) extends FlatTraverseArrs[R, T, I, O]

case class Leaf[R, T[_], I, O](arr: I => Eff[R, T[O]]) extends AnyVal with FlatTraverseArrs[R, T, I, O]

object FlatTraverseArrs {
  def singleton[R, T[_], I, O](arr: I => Eff[R, T[O]]): FlatTraverseArrs[R, T, I, O] =
    Leaf[R, T, I, O](arr)

  def compose[R, T[_], I, O, O2](first: FlatTraverseArrs[R, T, I, O], second: FlatTraverseArrs[R, T, O, O2]): FlatTraverseArrs[R, T, I, O2] = first match {
    case Encompose(arrsFirst) => second match {
      case Encompose(arrsSecond) => Encompose[R, T, I, O2](arrsFirst concatNev arrsSecond)
      case _: Leaf[R, T, O, O2] => Encompose[R, T, I, O2](NonEmptyVector.fromVectorUnsafe(arrsFirst.toVector.:+[FlatTraverseArrs[R, T, Any, Any], Vector[FlatTraverseArrs[R, T, Any, Any]]](second.asInstanceOf[FlatTraverseArrs[R, T, Any, Any]])))
    }
    case _: Leaf[R, T, I, O] => second match {
      case Encompose(arrsSecond) =>
        Encompose[R, T, I, O2](NonEmptyVector.fromVectorUnsafe(arrsSecond.toVector.+:[FlatTraverseArrs[R, T, Any, Any], Vector[FlatTraverseArrs[R, T, Any, Any]]](first.asInstanceOf[FlatTraverseArrs[R, T, Any, Any]])))
      case _: Leaf[R, T, O, O2] =>
        Encompose[R, T, I, O2](NonEmptyVector.of(first.asInstanceOf[FlatTraverseArrs[R, T, Any, Any]], second.asInstanceOf[FlatTraverseArrs[R, T, Any, Any]]))
    }

  }

  type _taskPar[R] = Member[TaskParallel, R]

  def run[R: _taskPar, T[_] : Traverse : Monad, I, O](rt: FlatTraverseArrs[R, T, I, O])(i: I): TaskParallel[Eff[R, T[O]]] = rt match {
    case Encompose(rts) =>
      rts.reduceLeftTo(rt => (i: Any) => run[R, T, Any, Any](rt)(i))((f, g) =>
        (i: Any) =>
          Task.now(suspend(f(i)).flatMap(ta => ta.traverseA[R, T[Any]]((a: Any) => suspend(run[R, T, Any, Any](g)(a)))).map(_.flatten)).parallel
      )(i).asInstanceOf[Task[Eff[R, T[O]]]].parallel
    case Leaf(arr) =>
      Task.now(arr(i)).parallel
  }

}
