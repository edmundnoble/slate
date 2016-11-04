package slate
package bench

import qq.data.{ConcreteFilter, FilterComponent, QQDSL}

import scala.annotation.tailrec

object Util {

  @tailrec
  def buildRec(transform: ConcreteFilter => ConcreteFilter, count: Int, start: ConcreteFilter): ConcreteFilter = {
    if (count == 0) start
    else buildRec(transform, count - 1, transform(start))
  }

  def composeBuildRec(i: Int, f: ConcreteFilter) =
    Util.buildRec(QQDSL.compose(_, f), i, f)

}
