package qq
package cc

import cats.data.NonEmptyVector
import cats.implicits._
import cats.{Monad, Traverse}
import monix.eval.Task
import org.atnos.eff.{Eff, Member}
import org.atnos.eff.monix.TaskEffect._
import org.atnos.eff.syntax.all._

/*
This is an experimental construct I created to address stack-safety problems with composing filters in QQ.
It essentially represents a tree of TraverseM arrows (I => Eff[R, T[O]]) which need to be composed.
It is VERY similar to the Arrs construct in Eff.
A RanTraverseM[R, T, I, O] is a function I => Eff[R, T[O]] with re-associated, trampolined composition.
 */
sealed trait RanTraverseM[R, T[_], I, O] {
  def apply(i: I)(implicit ev1: Traverse[T], ev2: Monad[T], ev3: Member[Task, R]): Eff[R, T[O]] =
    suspend(RanTraverseM.run[R, T, I, O](this)(i))
}
case class Encompose[R, T[_], I, O](arrs: NonEmptyVector[RanTraverseM[R, T, Any, Any]]) extends RanTraverseM[R, T, I, O]

case class Leaf[R, T[_], I, O](arr: I => Eff[R, T[O]]) extends RanTraverseM[R, T, I, O]

object RanTraverseM {
  def singleton[R, T[_], I, O](arr: I => Eff[R, T[O]]) = Leaf(arr)

  def compose[R, T[_], I, O, O2](first: RanTraverseM[R, T, I, O], second: RanTraverseM[R, T, O, O2]): RanTraverseM[R, T, I, O2] = first match {
    case Encompose(arrsFirst) => second match {
      case Encompose(arrsSecond) => Encompose[R, T, I, O2](arrsFirst concatNev arrsSecond)
      case _: Leaf[R, T, O, O2] => Encompose[R, T, I, O2](NonEmptyVector.fromVectorUnsafe(arrsFirst.toVector.:+[RanTraverseM[R, T, Any, Any], Vector[RanTraverseM[R, T, Any, Any]]](second.asInstanceOf[RanTraverseM[R, T, Any, Any]])))
    }
    case _: Leaf[R, T, I, O] => second match {
      case Encompose(arrsSecond) =>
        Encompose[R, T, I, O2](NonEmptyVector.fromVectorUnsafe(arrsSecond.toVector.+:[RanTraverseM[R, T, Any, Any], Vector[RanTraverseM[R, T, Any, Any]]](first.asInstanceOf[RanTraverseM[R, T, Any, Any]])))
      case _: Leaf[R, T, O, O2] =>
        Encompose[R, T, I, O2](NonEmptyVector.of(first.asInstanceOf[RanTraverseM[R, T, Any, Any]], second.asInstanceOf[RanTraverseM[R, T, Any, Any]]))
    }

  }

  def run[R: _task, T[_] : Traverse : Monad, I, O](rt: RanTraverseM[R, T, I, O])(i: I)(implicit ev: Member[Task, R]): Task[Eff[R, T[O]]] = rt match {
    case Encompose(rts) =>
      rts.reduceLeftTo(rt => (i: Any) => run[R, T, Any, Any](rt)(i))((f, g) =>
        (i: Any) => Task.now(suspend(f(i)).flatMap(ta => ta.traverseA[R, T[Any]]((a: Any) => suspend(run[R, T, Any, Any](g)(a)))).map(_.flatten))
      )(i).asInstanceOf[Task[Eff[R, T[O]]]]
    case Leaf(arr) =>
      Task.now(arr(i))
  }

}
