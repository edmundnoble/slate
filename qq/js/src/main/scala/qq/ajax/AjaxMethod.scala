package qq.ajax

sealed trait AjaxMethod

object AjaxMethod {
  case object PUT extends AjaxMethod
  case object POST extends AjaxMethod
  case object GET extends AjaxMethod
  case object PATCH extends AjaxMethod
  case object DELETE extends AjaxMethod
  @inline final def asString(meth: AjaxMethod): String = meth match {
    case GET => "GET"
    case PATCH => "PATCH"
    case POST => "POST"
    case PUT => "PUT"
    case DELETE => "DELETE"
  }
}
