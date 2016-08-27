package dash.bench

import qq.Filter

import scala.annotation.tailrec

object Util {

  @tailrec
  def buildRec(transform: Filter => Filter, count: Int, start: Filter): Filter = {
    if (count == 0) start
    else buildRec(transform, count - 1, transform(start))
  }

}
