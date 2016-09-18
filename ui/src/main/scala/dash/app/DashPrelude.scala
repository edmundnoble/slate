package dash
package app

import monix.eval.Task
import monix.scalaz._
import dash.ajax.{Ajax, AjaxMethod}
import org.scalajs.dom.XMLHttpRequest
import qq.cc.{QQRuntimeException, VarBindings}
import qq.data.CompiledDefinition
import qq.jsc._
import qq.util._
import upickle.Invalid

import scala.concurrent.duration._
import scala.scalajs.js
import scalaz.std.list._
import scalaz.\/
import scalaz.syntax.traverse._
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
          implicit val ajaxTimeout = Ajax.Timeout(1000.millis)
          val urlsTask = urlFilter(bindings)(jsv).flatMap(_.traverse {
            case s: String => Task.now(s)
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not a URL"))
          })
          val queryParamssTask = queryParamsFilter(bindings)(jsv).flatMap(_.traverse {
            case o: js.Object => Task.now(o.toDictionary)
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not query params/data"))
          })
          val datasTask = dataFilter(bindings)(jsv).flatMap(_.traverse {
            case s: String => Task.now(s)
            case o: js.Object => Task.now(Json.jsToString(o))
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not usable as POST data"))
          })
          val headerssTask = headersFilter(bindings)(jsv).flatMap(_.traverse {
            case o: js.Object => Task.now(o.toDictionary.asInstanceOf[js.Dictionary[String]])
            case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not headers"))
          })
          val ajaxs: Task[List[XMLHttpRequest]] =
            Task.zipMap4(urlsTask, datasTask, queryParamssTask.map(_.map(_.toMap)), headerssTask.map(_.map(_.toMap)))((urls, datas, queryParamss, headerss) =>
              for {url <- urls; data <- datas; queryParams <- queryParamss; headers <- headerss} yield
                Ajax(ajaxMethod, url, data, queryParams, headers, withCredentials = false, "")
            ).flatMap(_.sequence[Task, XMLHttpRequest])
          ajaxs.flatMap(
            _.traverse[Invalid.Json \/ ?, Any](resp => Json.stringToJs(resp.responseText)).fold(Task.raiseError(_), Task.now)
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
