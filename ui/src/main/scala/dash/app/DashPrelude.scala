package dash
package app

import dash.ajax.{Ajax, AjaxMethod}
import monix.eval.Task
import org.scalajs.dom.XMLHttpRequest
import qq.cc.{OrCompilationError, Prelude, QQRuntime, QQRuntimeException}
import qq.data.CompiledDefinition
import qq.jsc._
import qq.util._

import scala.concurrent.duration._
import scala.scalajs.js
import scalaz.syntax.either._
import scalaz.syntax.apply._
import monix.scalaz._

object DashPrelude extends Prelude[Any] {

  import CompiledDefinition.noParamDefinition

  def googleAuth: CompiledDefinition[Any] =
    noParamDefinition("googleAuth", _ => _ => identify.getAuthToken(interactive = false).map(_ :: Nil))

  def launchAuth: CompiledDefinition[Any] =
    CompiledDefinition[Any]("launchAuth", 1, CompiledDefinition.standardEffectDistribution[Any] {
      case List(url) => _ =>
        url match {
          case s: String => identify.launchWebAuthFlow(interactive = false, s).map(_ :: Nil)
          case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not a URL"))
        }
    })

  private def makeAjaxDefinition(name: String, ajaxMethod: AjaxMethod) = CompiledDefinition[Any](name, 4,
    CompiledDefinition.standardEffectDistribution[Any] {
      case List(urlRaw, queryParamsRaw, dataRaw, headersRaw) => _ =>
        implicit val ajaxTimeout = Ajax.Timeout(1000.millis)
        val urlTask = urlRaw match {
          case s: String => Task.now(s)
          case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not a URL"))
        }
        val queryParamsTask = queryParamsRaw match {
          case o: js.Object => Task.now(o.toDictionary.toMap)
          case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not query params/data"))
        }
        val dataTask = dataRaw match {
          case s: String => Task.now(s)
          case o: js.Object => Task.now(Json.jsToString(o))
          case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not usable as POST data"))
        }
        val headersTask = headersRaw match {
          case o: js.Object => Task.now(o.toDictionary.asInstanceOf[js.Dictionary[String]].toMap)
          case k => Task.raiseError(QQRuntimeException(JSRuntime.print(k) + " is not headers"))
        }
        val ajaxs: Task[XMLHttpRequest] =
          (urlTask |@| dataTask |@| queryParamsTask |@| headersTask)(
            Ajax(ajaxMethod, _, _, _, _, withCredentials = false, "")
          ).flatten
        ajaxs.flatMap(
          resp => Json.stringToJs(resp.responseText).fold(Task.raiseError(_), Task.now)
        )
    })

  def httpDelete: CompiledDefinition[Any] = makeAjaxDefinition("httpDelete", AjaxMethod.DELETE)

  def httpGet: CompiledDefinition[Any] = makeAjaxDefinition("httpGet", AjaxMethod.GET)

  def httpPost: CompiledDefinition[Any] = makeAjaxDefinition("httpPost", AjaxMethod.POST)

  def httpPatch: CompiledDefinition[Any] = makeAjaxDefinition("httpPatch", AjaxMethod.PATCH)

  def httpPut: CompiledDefinition[Any] = makeAjaxDefinition("httpPut", AjaxMethod.PUT)

  override def all(runtime: QQRuntime[Any]): OrCompilationError[IndexedSeq[CompiledDefinition[Any]]] =
    Vector(googleAuth, httpDelete, httpGet, httpPost, httpPatch, httpPut).right
}
