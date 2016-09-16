package qq

import scala.scalajs.js

package object jsc {

  implicit class objectOps(obj: js.Object) {
    def toDictionary: js.Dictionary[Any] = obj.asInstanceOf[js.Dictionary[Any]]
  }

}
