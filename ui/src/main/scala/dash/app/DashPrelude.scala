package dash
package app

import dash.ajax.{Ajax, AjaxMethod}
import monix.eval.{Coeval, Task}
import qq.cc.{OrCompilationError, Prelude, QQRuntime, QQRuntimeException}
import qq.data.{CompiledDefinition, JSON}
import qq.util._

import scala.concurrent.duration._
import scalaz.syntax.either._
import scalaz.syntax.apply._
import monix.scalaz._
import qq.Json
import qq.Platform.Rec._

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

  override def all(runtime: QQRuntime[JSON]): OrCompilationError[IndexedSeq[CompiledDefinition[JSON]]] =
    Vector(googleAuth, launchAuth, httpDelete, httpGet, httpPost, httpPatch, httpPut).right
}
