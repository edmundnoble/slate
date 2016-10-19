package dash
package app

import dash.ajax.{Ajax, AjaxException, AjaxMethod}
import monix.eval.{Coeval, Task}
import monix.cats._
import org.scalajs.dom.XMLHttpRequest
import qq.Json
import qq.Platform.Rec._
import qq.cc.{CompiledFilter, OrCompilationError, Prelude, QQRuntimeError, QQRuntimeException}
import qq.data.{CompiledDefinition, JSON}
import qq.util.Recursion.RecursionEngine
import qq.util._

import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.Any
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._

object DashPrelude extends Prelude {

  import QQRuntimeException._
  import CompiledDefinition.noParamDefinition

  def googleAuth: CompiledDefinition =
    noParamDefinition("googleAuth", _ => _ => identify.getAuthToken(interactive = true).map(s => (JSON.Str(s) :: Nil).validNel))

  def launchAuth: CompiledDefinition =
    CompiledDefinition("launchAuth", 2, CompiledDefinition.standardEffectDistribution {
      case List(urlRaw, queryParamsRaw) => _ =>
        val urlVerified: ValidatedNel[QQRuntimeError, String] = urlRaw match {
          case JSON.Str(s) => s.validNel
          case k => typeError("ajax", "object" -> k).invalidNel
        }
        val queryParamsVerified: ValidatedNel[QQRuntimeError, Map[String, Any]] = queryParamsRaw match {
          case o: JSON.Obj => o.toMap.value.mapValues(Json.JSONToJsRec(_)).validNel
          case k => typeError("ajax", "object" -> k).invalidNel
        }
        val urlWithQueryParams = (urlVerified |@| queryParamsVerified).map (Ajax.addQueryParams)
        for {
          webAuthResult <- urlWithQueryParams.traverse(identify.launchWebAuthFlow(interactive = true, _))
          accessToken = webAuthResult.map(r => r.substring(r.indexOf("&code=") + "&code=".length))
        } yield accessToken.map(t => JSON.Obj("code" -> JSON.Str(t)))
    })

  private def makeAjaxDefinition(name: String, ajaxMethod: AjaxMethod) = CompiledDefinition(name, 4,
    CompiledDefinition.standardEffectDistribution {
      case List(urlRaw, queryParamsRaw, dataRaw, headersRaw) => _ =>
        implicit val ajaxTimeout = Ajax.Timeout(2000.millis)
        val urlValidated: ValidatedNel[QQRuntimeError, String] = urlRaw match {
          case JSON.Str(s) => s.validNel
          case k => typeError("ajax", "string" -> k).invalidNel
        }
        val queryParamsValidated: ValidatedNel[QQRuntimeError, Map[String, Any]] = queryParamsRaw match {
          case o: JSON.Obj => o.toMap.value.mapValues(Json.JSONToJsRec(_)).validNel
          case k => typeError("ajax", "object" -> k).invalidNel
        }
        val dataValidated: ValidatedNel[QQRuntimeError, String] = dataRaw match {
          case JSON.Str(s) => s.validNel
          case o: JSON.Obj => JSON.render(o).validNel
          case k => typeError("ajax", "string | object" -> k).invalidNel
        }
        val headersValidated: ValidatedNel[QQRuntimeError, Map[String, String]] = headersRaw match {
          case o: JSON.ObjList if o.value.forall(_._2.isInstanceOf[JSON.Str]) => o.toMap.value.mapValues(_.asInstanceOf[JSON.Str].value).validNel
          case o: JSON.ObjMap if o.value.forall(_._2.isInstanceOf[JSON.Str]) => o.toMap.value.mapValues(_.asInstanceOf[JSON.Str].value).validNel
          case k => typeError("ajax", "object" -> k).invalidNel
        }
        for {
          resp <- ((urlValidated |@| dataValidated |@| queryParamsValidated |@| headersValidated).map (
            Ajax(ajaxMethod, _, _, _, _, withCredentials = false, "").onErrorRestart(1).map(_.validNel[QQRuntimeError]).onErrorHandle[ValidatedNel[QQRuntimeError, XMLHttpRequest]] {
              case e: QQRuntimeException => e.errors.invalid[XMLHttpRequest]
            }
          )).sequence
          asJson <- resp.flatten.traverse(r => Json.stringToJSON(r.responseText).fold(Task.raiseError(_), Task.now(_)))
        } yield asJson
    })

  def httpDelete: CompiledDefinition = makeAjaxDefinition("httpDelete", AjaxMethod.DELETE)

  def httpGet: CompiledDefinition = makeAjaxDefinition("httpGet", AjaxMethod.GET)

  def httpPost: CompiledDefinition = makeAjaxDefinition("httpPost", AjaxMethod.POST)

  def httpPatch: CompiledDefinition = makeAjaxDefinition("httpPatch", AjaxMethod.PATCH)

  def httpPut: CompiledDefinition = makeAjaxDefinition("httpPut", AjaxMethod.PUT)

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

  def nowRFC3339: CompiledDefinition = noParamDefinition("nowRFC3339", CompiledFilter.func { _ =>
    val now = new js.Date()
    Task.now((JSON.Str(toRFC3339(now)) :: Nil).validNel)
  })

  def formatDatetimeFriendly: CompiledDefinition = noParamDefinition("formatDatetimeFriendly", CompiledFilter.func {
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
      Task.now((JSON.Str(fuzzy) :: Nil).validNel[QQRuntimeError])
    case k =>
      Task.now((typeError("formatDatetimeFriendly", "string" -> k): QQRuntimeError).invalidNel[List[JSON]])
  })

  override def all(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition]] =
    Vector(googleAuth, launchAuth, httpDelete, httpGet, httpPost, httpPatch, httpPut, nowRFC3339, formatDatetimeFriendly).right
}
