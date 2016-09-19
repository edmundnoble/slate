package dash
package bench

import matryoshka.{Corecursive, Recursive}
import qq.data.{FilterComponent, QQDSL}

import scala.annotation.tailrec

object Util {

  @tailrec
  def buildRec[T[_[_]] : Recursive : Corecursive](transform: T[FilterComponent] => T[FilterComponent], count: Int, start: T[FilterComponent]): T[FilterComponent] = {
    if (count == 0) start
    else buildRec(transform, count - 1, transform(start))
  }

  def composeBuildRec[T[_[_]] : Recursive : Corecursive](i: Int, f: T[FilterComponent]) =
    Util.buildRec[T](QQDSL.dsl[T].compose(_, f), i, f)

}
