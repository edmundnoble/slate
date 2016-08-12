package qq

import upickle.Js

package object json {
  def read(s: String): Js.Value = {
    ???
    /*
        jawn.Parser.parseFromString(s)(json.JawnFacade) match {
          case util.Success(v) => v
          case util.Failure(e) => throw Js.Invalid.Json(e.toString, s)
        }
    */
  }
}
