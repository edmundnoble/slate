package dash
package app

import java.text.SimpleDateFormat
import java.util.Date

import dash.ajax.{Ajax, AjaxMethod}
import monix.eval.{Coeval, Task}
import qq.cc.{CompiledFilter, OrCompilationError, Prelude, QQRuntime, QQRuntimeException}
import qq.data.{CompiledDefinition, JSON}
import qq.util._

import scala.concurrent.duration._
import scalaz.syntax.either._
import scalaz.syntax.apply._
import monix.scalaz._
import qq.Json
import qq.Platform.Rec._
import qq.util.Recursion.RecursionEngine

import scala.scalajs.js

object DashPrelude extends Prelude[JSON] {

  import CompiledDefinition.noParamDefinition

  def googleAuth: CompiledDefinition[JSON] =
    noParamDefinition("googleAuth", _ => _ => identify.getAuthToken(interactive = true).map(JSON.Str(_) :: Nil))

  def launchAuth: CompiledDefinition[JSON] =
    CompiledDefinition[JSON]("launchAuth", 2, CompiledDefinition.standardEffectDistribution[JSON] {
      case List(urlRaw, queryParamsRaw) => _ =>
        val urlCoeval = urlRaw match {
          case JSON.Str(s) => Coeval.now(s)
          case k => Coeval.raiseError(QQRuntimeException(Json.jsonToString(k) + " is not a URL"))
        }
        val queryParamsCoeval = queryParamsRaw match {
          case o: JSON.Obj => Coeval.now(o.toMap.value.mapValues(Json.JSONToJsRec(_)))
          case k => Coeval.raiseError(QQRuntimeException(Json.jsonToString(k) + " is not a query params object"))
        }
        for {
          urlWithQueryParams <- Task.coeval((urlCoeval |@| queryParamsCoeval) (Ajax.addQueryParams))
          webAuthResult <- identify.launchWebAuthFlow(interactive = true, urlWithQueryParams)
          accessToken = webAuthResult.substring(webAuthResult.indexOf("&code=") + "&code=".length)
        } yield JSON.Obj("code" -> JSON.Str(accessToken))
    })

  private def makeAjaxDefinition(name: String, ajaxMethod: AjaxMethod) = CompiledDefinition[JSON](name, 4,
    CompiledDefinition.standardEffectDistribution[JSON] {
      case List(urlRaw, queryParamsRaw, dataRaw, headersRaw) => _ =>
        implicit val ajaxTimeout = Ajax.Timeout(2000.millis)
        val urlCoeval = urlRaw match {
          case JSON.Str(s) => Coeval.now(s)
          case k => Coeval.raiseError(QQRuntimeException(Json.jsonToString(k) + " is not a URL"))
        }
        val queryParamsCoeval = queryParamsRaw match {
          case o: JSON.Obj => Coeval.now(o.toMap.value.mapValues(Json.JSONToJsRec(_)))
          case k => Coeval.raiseError(QQRuntimeException(Json.jsonToString(k) + " is not query params/data"))
        }
        val dataCoeval = dataRaw match {
          case JSON.Str(s) => Coeval.now(s)
          case o: JSON.Obj => Coeval.now(JSON.render(o))
          case k => Coeval.raiseError(QQRuntimeException(Json.jsonToString(k) + " is not usable as POST data"))
        }
        val headersCoeval = headersRaw match {
          case o: JSON.ObjList if o.value.forall(_._2.isInstanceOf[JSON.Str]) => Coeval.now(o.toMap.value.mapValues(_.asInstanceOf[JSON.Str].value))
          case o: JSON.ObjMap if o.value.forall(_._2.isInstanceOf[JSON.Str]) => Coeval.now(o.toMap.value.mapValues(_.asInstanceOf[JSON.Str].value))
          case k => Coeval.raiseError(QQRuntimeException(Json.jsonToString(k) + " is not headers"))
        }
        for {
          resp <- Task.coeval((urlCoeval |@| dataCoeval |@| queryParamsCoeval |@| headersCoeval) (
            Ajax(ajaxMethod, _, _, _, _, withCredentials = false, "").onErrorRestart(1)
          )).flatten
          asJson <- Json.stringToJSON(resp.responseText).fold(Task.raiseError(_), Task.now)
        } yield asJson
    })

  def httpDelete: CompiledDefinition[JSON] = makeAjaxDefinition("httpDelete", AjaxMethod.DELETE)

  def httpGet: CompiledDefinition[JSON] = makeAjaxDefinition("httpGet", AjaxMethod.GET)

  def httpPost: CompiledDefinition[JSON] = makeAjaxDefinition("httpPost", AjaxMethod.POST)

  def httpPatch: CompiledDefinition[JSON] = makeAjaxDefinition("httpPatch", AjaxMethod.PATCH)

  def httpPut: CompiledDefinition[JSON] = makeAjaxDefinition("httpPut", AjaxMethod.PUT)

  final def toRFC3339(d: js.Date): String = {
    def pad(n: Int): String = {
      val toStr = n.toString
      if (n < 10) "0" + toStr else toStr
    }
    d.getUTCFullYear() + "-" +
      pad(d.getUTCMonth() + 1) + "-" + pad(d.getUTCDate()) + "T" +
      pad(d.getUTCHours()) + ":" +
      pad(d.getUTCMinutes()) + ":" +
      pad(d.getUTCSeconds()) + "Z"
  }

  def nowRFC3339: CompiledDefinition[JSON] = noParamDefinition[JSON]("nowRFC3339", CompiledFilter.func[JSON] { _ =>
    val now = new js.Date()
    Task.now(JSON.Str(toRFC3339(now)) :: Nil)
  })

  def formatDatetimeFriendly: CompiledDefinition[JSON] = noParamDefinition[JSON]("formatDatetimeFriendly", CompiledFilter.func[JSON] {
    case JSON.Str(s) =>
      val asDate = js.Date.parse(s)
      // Make a fuzzy time
      val delta = Math.round((asDate - new js.Date().getTime()) / 1000)

      val minute = 60
      val hour = minute * 60
      val day = hour * 24
      val week = day * 7

      val fuzzy =
        if (delta < 30) {
           "just then"
        } else if (delta < minute) {
           delta.toString + " seconds ago"
        } else if (delta < 2 * minute) {
           "in a minute"
        } else if (delta < hour) {
           Math.floor(delta / minute).toString + " minutes ago"
        } else if (Math.floor(delta / hour) == 1) {
          "in 1 hour"
        } else if (delta < day) {
           "in " + Math.floor(delta / hour).toString + " hours"
        } else if (delta < day * 2) {
           "tomorrow"
        } else if (delta < week) {
          "in " + Math.floor(delta / day) + " days"
        } else {
          "in " + Math.floor(delta / week) + " weeks"
        }
      Task.now(JSON.Str(fuzzy) :: Nil)
    case k =>
      Task.raiseError(QQRuntimeException("Can't format " + print(k) + " as a RFC3339 datetime"))
  })

  override def all(runtime: QQRuntime[JSON])(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition[JSON]]] =
    Vector(googleAuth, launchAuth, httpDelete, httpGet, httpPost, httpPatch, httpPut, nowRFC3339, formatDatetimeFriendly).right
}
