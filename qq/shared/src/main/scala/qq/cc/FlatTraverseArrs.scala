package qq
package cc

import cats.data.NonEmptyVector
import cats.implicits._
import cats.{Monad, Traverse}
import monix.eval.Task
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import qq.cc.FlatTraverseArrs._taskPar
import qq.util._

/*
This is an experimental construct I created to address stack-safety problems with composing filters in QQ.
It essentially represents a tree of TraverseM arrows (I => Eff[R, T[O]]) which need to be composed.
It is VERY similar to the Arrs construct in Eff.
A FlatTraverseArrs[R, T, I, O] is a function I => Eff[R, T[O]] with re-associated, trampolined composition.
The trampolining is performed inside the result effect; that's the real magic.
 */
sealed trait FlatTraverseArrs[R, T[_], I, O] {
  def apply(i: I)(implicit T: Traverse[T], TM: Monad[T], par: _taskPar[R]): Eff[R, T[O]] =
    suspend(FlatTraverseArrs.run(this)(i))
}

case class Encompose[R, T[_], I, O](arrs: NonEmptyVector[FlatTraverseArrs[R, T, Any, Any]]) extends FlatTraverseArrs[R, T, I, O]

case class Leaf[R, T[_], I, O](arr: I => Eff[R, T[O]]) extends FlatTraverseArrs[R, T, I, O]

case object Empty extends FlatTraverseArrs[Nothing, Nothing, Any, Nothing]

object FlatTraverseArrs {
  def empty[R, T[_], I]: FlatTraverseArrs[R, T, I, I] =
    Empty.asInstanceOf[FlatTraverseArrs[R, T, I, I]]

  def singleton[R, T[_], I, O](arr: I => Eff[R, T[O]]): FlatTraverseArrs[R, T, I, O] =
    Leaf[R, T, I, O](arr)

  def fromEndoFuns[R, T[_] : Traverse, I](vec: T[FlatTraverseArrs[R, T, I, I]]): FlatTraverseArrs[R, T, I, I] = {
    if (vec.isEmpty) empty
    else Encompose(NonEmptyVector.fromVectorUnsafe(vec.asInstanceOf[Vector[FlatTraverseArrs[R, T, Any, Any]]]))
  }

  def fromEndoFunsL[R, T[_] : Traverse, I](vec: Vector[I => Eff[R, T[I]]]): FlatTraverseArrs[R, T, I, I] = {
    if (vec.isEmpty) empty
    else Encompose(NonEmptyVector.fromVectorUnsafe(vec.map(Leaf[R, T, I, I]).asInstanceOf[Vector[FlatTraverseArrs[R, T, Any, Any]]]))
  }

  def compose[R, T[_], I, O, O2](first: FlatTraverseArrs[R, T, I, O], second: FlatTraverseArrs[R, T, O, O2]): FlatTraverseArrs[R, T, I, O2] = first match {
    case Encompose(arrsFirst) => second match {
      case Encompose(arrsSecond) => Encompose[R, T, I, O2](arrsFirst concatNev arrsSecond)
      case _: Leaf[R, T, O, O2] => Encompose[R, T, I, O2](NonEmptyVector.fromVectorUnsafe(arrsFirst.toVector.:+[FlatTraverseArrs[R, T, Any, Any], Vector[FlatTraverseArrs[R, T, Any, Any]]](second.asInstanceOf[FlatTraverseArrs[R, T, Any, Any]])))
      case _ => first.asInstanceOf[FlatTraverseArrs[R, T, I, O2]]
    }
    case _: Leaf[R, T, I, O] => second match {
      case Encompose(arrsSecond) =>
        Encompose[R, T, I, O2](NonEmptyVector.fromVectorUnsafe(arrsSecond.toVector.+:[FlatTraverseArrs[R, T, Any, Any], Vector[FlatTraverseArrs[R, T, Any, Any]]](first.asInstanceOf[FlatTraverseArrs[R, T, Any, Any]])))
      case _: Leaf[R, T, O, O2] =>
        Encompose[R, T, I, O2](NonEmptyVector.of(first.asInstanceOf[FlatTraverseArrs[R, T, Any, Any]], second.asInstanceOf[FlatTraverseArrs[R, T, Any, Any]]))
      case _ => first.asInstanceOf[FlatTraverseArrs[R, T, I, O2]]
    }
    case _ => second.asInstanceOf[FlatTraverseArrs[R, T, I, O2]]

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
    case _ =>
      Task.now(Monad[T].pure(i.asInstanceOf[O]).pureEff[R]).parallel
  }

}
