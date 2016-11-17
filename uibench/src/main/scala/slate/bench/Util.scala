package slate
package bench

import qq.data.{FilterAST, QQDSL}

import scala.annotation.tailrec

object Util {

  @tailrec
  def buildRec(transform: FilterAST => FilterAST, count: Int, start: FilterAST): FilterAST = {
    if (count == 0) start
    else buildRec(transform, count - 1, transform(start))
  }

  def composeBuildRec(i: Int, f: FilterAST) =
    Util.buildRec(QQDSL.compose(_, f), i, f)

}
