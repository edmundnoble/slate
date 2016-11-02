package qq.cc

import cats.data.NonEmptyVector
import cats.implicits._
import cats.{Eval, Foldable, Monad, Traverse}
import org.atnos.eff.{Eff, Member, eval}
import org.atnos.eff.syntax.all._

sealed trait RanTraverseM[R, T[_], I, O] {
  def apply(i: I)(implicit ev1: Traverse[T], ev2: Monad[T], ev3: Member[Eval, R]): Eff[R, T[O]] =
    RanTraverseM.run[R, T, I, O](this)(i)
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

  def run[R, T[_]: Traverse : Monad, I, O](rt: RanTraverseM[R, T, I, O])(i: I)(implicit ev: Member[Eval, R]): Eff[R, T[O]] = rt match {
    case Encompose(rts) =>
      eval.delay[R, Eff[R, T[O]]] {
        val r: Eff[R, T[Any]] =
          rts.reduceLeftTo(run[R, T, Any, Any])((f, g) => (i: Any) => f(i).flatMap(ta => ta.traverseA[R, T[Any]]((a: Any) => run[R, T, Any, Any](g)(a))).map(_.flatten))(i)
        r.asInstanceOf[Eff[R, T[O]]]
      }.flatten
    case Leaf(arr) =>
      arr(i)
  }

}
