package qq.data

import scala.language.higherKinds
import cats.{Applicative, Eval, Traverse}

sealed abstract class PathComponent

// Collecting the results from a filter which returns JSON arrays
// yields a filter which concatenates the arrays' values into a single list of output values
// Inverse of EnlistFilter.
case object CollectResults extends PathComponent

// Select key, index or range in JSON object or array.
// Return null if asked for something not contained in the target.
final case class SelectKey(key: String) extends PathComponent

final case class SelectIndex(index: Int) extends PathComponent

final case class SelectRange(start: Int, end: Int) extends PathComponent

sealed abstract class PathOperationF[+A]

case object PathGet extends PathOperationF[Nothing]
case class PathModify[A](value: A) extends PathOperationF[A]
case class PathSet[A](value: A) extends PathOperationF[A]

object PathOperationF {
  implicit def pathOperationTraverse: Traverse[PathOperationF] = new Traverse[PathOperationF] {
    override def traverse[G[_], A, B](fa: PathOperationF[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[PathOperationF[B]] = fa match {
      case _: PathGet.type => G.pure(PathGet)
      case PathSet(v) => G.map(f(v))(PathSet(_))
      case PathModify(v) => G.map(f(v))(PathModify(_))
    }
    override def foldLeft[A, B](fa: PathOperationF[A], b: B)(f: (B, A) => B): B = fa match {
      case _: PathGet.type => b
      case PathSet(v) => f(b, v)
      case PathModify(v) => f(b, v)
    }
    override def foldRight[A, B](fa: PathOperationF[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case _: PathGet.type => b
      case PathSet(v) => f(v, b)
      case PathModify(v) => f(v, b)
    }
  }
}
