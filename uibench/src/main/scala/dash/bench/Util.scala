package dash
package bench

import matryoshka.{Corecursive, Recursive}
import qq.FilterComponent

import scala.annotation.tailrec

object Util {

  @tailrec
  def buildRec[T[_[_]]: Recursive: Corecursive](transform: T[FilterComponent] => T[FilterComponent], count: Int, start: T[FilterComponent]): T[FilterComponent] = {
    if (count == 0) start
    else buildRec(transform, count - 1, transform(start))
  }

}
