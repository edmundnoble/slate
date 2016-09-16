package dash
package app

import monix.eval.Task
import monix.scalaz._
import qq.QQCompiler.VarBindings
import qq.ajax.{Ajax, AjaxMethod}
import qq.jsc._
import qq.{CompiledDefinition, QQRuntimeException}

import scala.concurrent.duration._
import scala.scalajs.js
import scalaz.syntax.apply._
import scalaz.syntax.either._

object DashPrelude {

  import CompiledDefinition.noParamDefinition

  def googleAuth: CompiledDefinition[Any] =
    noParamDefinition("googleAuth", _ => _ => identify.getAuthToken(interactive = false).map(_ :: Nil))

  def launchAuth: CompiledDefinition[Any] =
    CompiledDefinition[Any]("launchAuth", 1, {
      case List(urlFilter) =>
        ((bindings: VarBindings[Any]) => (jsv: Any) =>
          urlFilter(bindings)(jsv).flatMap(_.head match {
            case s: String => identify.launchWebAuthFlow(interactive = false, s).map(_ :: Nil)
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not a URL"))
          })
          ).right
    })

  private def makeAjaxDefinition(name: String, ajaxMethod: AjaxMethod) = CompiledDefinition[Any](name, 4, {
    case List(urlFilter, queryParamsFilter, dataFilter, headersFilter) => {
      (bindings: VarBindings[Any]) =>
        (jsv: Any) =>
          val url = urlFilter(bindings)(jsv).flatMap(_.head match {
            case s: String => Task.now(s)
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not a URL"))
          })
          val queryParams = queryParamsFilter(bindings)(jsv).flatMap(_.head match {
            case o: js.Object => Task.now(o.toDictionary)
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not query params/data"))
          })
          val data = dataFilter(bindings)(jsv).flatMap(_.head match {
            case s: String => Task.now(s)
            case o: js.Object => Task.now(Json.jsToString(o))
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not usable as POST data"))
          })
          val headers = headersFilter(bindings)(jsv).flatMap(_.head match {
            case o: js.Object => Task.now(o.toDictionary.asInstanceOf[js.Dictionary[String]])
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not headers"))
          })
          val ajax =
            (url |@| data |@| queryParams.map(_.toMap) |@| headers.map(_.toMap)) (
              Ajax(ajaxMethod, _, _, _, _, withCredentials = false, "")(Ajax.Timeout(1000.millis))
            ).flatten
          ajax.map(resp =>
            List(Json.stringToJs(resp.responseText).fold(Task.raiseError, Task.now))
          )
    }.right
  })

  def httpDelete: CompiledDefinition[Any] = makeAjaxDefinition("httpDelete", AjaxMethod.DELETE)

  def httpGet: CompiledDefinition[Any] = makeAjaxDefinition("httpGet", AjaxMethod.GET)

  def httpPost: CompiledDefinition[Any] = makeAjaxDefinition("httpPost", AjaxMethod.POST)

  def httpPatch: CompiledDefinition[Any] = makeAjaxDefinition("httpPatch", AjaxMethod.PATCH)

  def httpPut: CompiledDefinition[Any] = makeAjaxDefinition("httpPut", AjaxMethod.PUT)


  val all = Vector(googleAuth, httpDelete, httpGet, httpPost, httpPatch, httpPut)
}
