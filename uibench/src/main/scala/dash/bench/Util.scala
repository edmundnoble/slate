package dash
package bench

import qq.ConcreteFilter

import scala.annotation.tailrec

object Util {

  @tailrec
  def buildRec(transform: ConcreteFilter => ConcreteFilter, count: Int, start: ConcreteFilter): ConcreteFilter = {
    if (count == 0) start
    else buildRec(transform, count - 1, transform(start))
  }

}
