package dash
package app

import monix.eval.Task
import monix.scalaz._
import qq.QQCompiler.BindingsByName
import qq.{CompiledDefinition, QQRuntimeException}
import qq.ajax.{Ajax, AjaxMethod}
import qq.jsc.{JSRuntime, Json}
import com.thoughtworks.each.Monadic._
import scala.concurrent.duration._
import scalaz.syntax.either._

import scala.scalajs.js

object DashPrelude {

  import CompiledDefinition.noParamDefinition

  def googleAuth: CompiledDefinition[Any] =
    noParamDefinition("googleAuth", _ => _ => identify.getAuthToken(interactive = false).map(_ :: Nil))

  private def makeAjaxDefinition(name: String, ajaxMethod: AjaxMethod) = CompiledDefinition[Any](name, 4, {
    case List(urlFilter, queryParamsFilter, dataFilter, headersFilter) => (
      (bindings: BindingsByName[Any]) => (jsv: Any) =>
        monadic[Task] {
          val url = (urlFilter(bindings)(jsv).each.head match {
            case s: String => Task.now(s)
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not a URL"))
          }).each
          val queryParams = (queryParamsFilter(bindings)(jsv).each.head match {
            case o: js.Object => Task.now(o)
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not query params/data"))
          }).each.asInstanceOf[js.Dictionary[js.Any]]
          val data = (dataFilter(bindings)(jsv).each.head match {
            case s: String => Task.now(s)
            case o: js.Object => Task.now(Json.jsToString(o))
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not usable as POST data"))
          }).each
          val headers = (headersFilter(bindings)(jsv).each.head match {
            case o: js.Object => Task.now(o)
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not headers"))
          }).each.asInstanceOf[js.Dictionary[String]]
          val ajax = (if (ajaxMethod == AjaxMethod.POST) {
            Ajax.post(url, data, queryParams.toMap, headers.toMap)(Ajax.Timeout(1000.millis))
          } else {
            Ajax(ajaxMethod, url, data, queryParams.toMap, headers.toMap, false, "")(Ajax.Timeout(1000.millis))
          }).each
          List(Json.stringToJs(ajax.responseText).fold(Task.raiseError, Task.now).each)
        }
      ).right
  })

  def httpDelete: CompiledDefinition[Any] = makeAjaxDefinition("httpDelete", AjaxMethod.DELETE)

  def httpGet: CompiledDefinition[Any] = makeAjaxDefinition("httpGet", AjaxMethod.GET)

  def httpPost: CompiledDefinition[Any] = makeAjaxDefinition("httpPost", AjaxMethod.POST)

  def httpPatch: CompiledDefinition[Any] = makeAjaxDefinition("httpPatch", AjaxMethod.PATCH)

  def httpPut: CompiledDefinition[Any] = makeAjaxDefinition("httpPut", AjaxMethod.PUT)


  val all = Vector(googleAuth, httpDelete, httpGet, httpPost, httpPatch, httpPut)
}
