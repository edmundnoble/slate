package qq.data

import scala.language.higherKinds
import scalaz.{Applicative, Traverse}

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

sealed abstract class PathOperationF[A]

case class PathGet[A]() extends PathOperationF[A]
case class PathModify[A](value: A) extends PathOperationF[A]
case class PathSet[A](value: A) extends PathOperationF[A]

object PathOperationF {
  implicit def pathOperationTraverse: Traverse[PathOperationF] = new Traverse[PathOperationF] {
    override def traverseImpl[G[_], A, B](fa: PathOperationF[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[PathOperationF[B]] = fa match {
      case g@PathGet() => G.point(g.asInstanceOf[PathOperationF[B]])
      case PathSet(v) => G.map(f(v))(PathSet(_))
      case PathModify(v) => G.map(f(v))(PathModify(_))
    }
  }
}
