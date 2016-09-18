package qq

import scala.scalajs.js

package object jsc {
  implicit class objectOps(val obj: js.Object) extends AnyVal {
    def toDictionary: js.Dictionary[Any] = obj.asInstanceOf[js.Dictionary[Any]]
  }
}
