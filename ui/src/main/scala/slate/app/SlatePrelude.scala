package slate
package app

import cats.Applicative
import cats.data.Validated
import cats.implicits._
import monix.eval.Task
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.scalajs.dom.XMLHttpRequest
import qq.Json
import qq.Platform.Rec._
import qq.cc.{CompiledFilter, CompiledFilterStack, OrCompilationError, OrRuntimeErr, Prelude, QQRuntimeError, QQRuntimeException, RuntimeErrs, TypeError}
import qq.data.{CompiledDefinition, JSON}
import qq.util.Recursion.RecursionEngine
import qq.util._
import slate.ajax.{Ajax, AjaxMethod}

import scala.concurrent.duration._
import scala.scalajs.js

object SlatePrelude extends Prelude {

  import CompiledDefinition.noParamDefinition
  import QQRuntimeException._

  def googleAuth: CompiledDefinition =
    noParamDefinition("googleAuth",
      CompiledFilter.constE(identify.getAuthToken(interactive = true).map[List[JSON]](JSON.Str(_) :: Nil).parallel.send[CompiledFilterStack]))

  def launchAuth: CompiledDefinition =
    CompiledDefinition("launchAuth", 2, CompiledDefinition.standardEffectDistribution {
      params => _ =>
        val urlRaw = params.head
        val queryParamsRaw = params.tail.head
        val urlVerified: Validated[RuntimeErrs, String] = urlRaw match {
          case JSON.Str(s) => s.validNel
          case k => (TypeError("ajax", "object" -> k): QQRuntimeError).invalidNel
        }
        val queryParamsVerified: Validated[RuntimeErrs, JSON.ObjList] = queryParamsRaw match {
          case o: JSON.ObjMap => JSON.ObjList(o.value.toList).validNel
          case o: JSON.ObjList => o.validNel
          case k => (TypeError("ajax", "object" -> k): QQRuntimeError).invalidNel
        }
        val urlWithQueryParams = Applicative[Validated[RuntimeErrs, ?]].map2(urlVerified, queryParamsVerified)(Ajax.addQueryParams)
        for {
          webAuthResult <-
          Eff.send[OrRuntimeErr, CompiledFilterStack, String](urlWithQueryParams.toEither)
            .flatMap(identify.launchWebAuthFlow(interactive = true, _).parallel.send[CompiledFilterStack])
          accessToken = webAuthResult.substring(webAuthResult.indexOf("&code=") + "&code=".length)
        } yield JSON.obj("code" -> JSON.Str(accessToken)) :: Nil
    })

  private def makeAjaxDefinition(name: String, ajaxMethod: AjaxMethod) = CompiledDefinition(name, 4,
    CompiledDefinition.standardEffectDistribution {
      params =>  _ =>
        val urlRaw = params.head
        val queryParamsRaw = params.tail.head
        val dataRaw = params.tail.tail.head
        val headersRaw = params.tail.tail.tail.head
        type Stack = Fx.fx2[TaskParallel, OrRuntimeErr]
        implicit val ajaxTimeout = Ajax.Timeout(2000.millis)
        val urlValidated: Validated[RuntimeErrs, String] = urlRaw match {
          case JSON.Str(s) => s.validNel
          case k => (TypeError("ajax", "string" -> k): QQRuntimeError).invalidNel
        }
        val queryParamsValidated: Validated[RuntimeErrs, JSON.ObjList] = queryParamsRaw match {
          case o: JSON.ObjMap => JSON.ObjList(o.value.toList).validNel
          case o: JSON.ObjList => o.validNel
          case k => (TypeError("ajax", "object" -> k): QQRuntimeError).invalidNel
        }
        val dataValidated: Validated[RuntimeErrs, String] = dataRaw match {
          case JSON.Str(s) => s.validNel
          case o: JSON.Obj => JSON.render(o).validNel
          case k => (TypeError("ajax", "string | object" -> k): QQRuntimeError).invalidNel
        }
        val headersValidated: Validated[RuntimeErrs, Map[String, String]] = headersRaw match {
          case o: JSON.ObjList if o.value.forall(_._2.isInstanceOf[JSON.Str]) => o.toMap.value.mapValues(_.asInstanceOf[JSON.Str].value).validNel
          case o: JSON.ObjMap if o.value.forall(_._2.isInstanceOf[JSON.Str]) => o.toMap.value.mapValues(_.asInstanceOf[JSON.Str].value).validNel
          case k => (TypeError("ajax", "object" -> k): QQRuntimeError).invalidNel
        }
        Eff.collapse[Stack, TaskParallel, List[JSON]](for {
          resp <-
          Eff.collapse[Stack, OrRuntimeErr, XMLHttpRequest](
            (urlValidated |@| dataValidated |@| queryParamsValidated |@| headersValidated).map(
              Ajax(ajaxMethod, _, _, _, _, withCredentials = false, "")
                .onErrorRestart(1)
                .map(Either.right)
                .onErrorHandle[OrRuntimeErr[XMLHttpRequest]] {
                case e: QQRuntimeException => Either.left[RuntimeErrs, XMLHttpRequest](e.errors)
              }.parallel
            ).toEither.sequence[TaskParallel, OrRuntimeErr[XMLHttpRequest]].map(_.flatten).parallel.send[Stack]
          )
          asJson = Json.stringToJSON(resp.responseText).fold(Task.raiseError(_), t => Task.now(t :: Nil)).parallel
        } yield asJson).into[CompiledFilterStack]
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

  def nowRFC3339: CompiledDefinition =
    noParamDefinition("nowRFC3339",
      CompiledFilter.constE(Task.eval(JSON.str(toRFC3339(new js.Date())) :: Nil).parallel.send[CompiledFilterStack]))

  // TODO: remove from here and AppView
  def formatDatetimeFriendlyImpl(d: js.Date): String = {
    // Make a fuzzy time
    val delta = Math.round((d.getTime() - new js.Date().getTime()) / 1000)

    val minute = 60
    val hour = minute * 60
    val day = hour * 24
    val week = day * 7

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
  }

  def formatDatetimeFriendly: CompiledDefinition = noParamDefinition("formatDatetimeFriendly", CompiledFilter.singleton {
    case JSON.Str(s) =>
      val asDate = js.Date.parse(s)
      val fuzzy = formatDatetimeFriendlyImpl(new js.Date(asDate))
      (JSON.str(fuzzy) :: Nil).pureEff[CompiledFilterStack]
    case k =>
      typeErrorE[CompiledFilterStack, List[JSON]]("formatDatetimeFriendly", "string" -> k)
  })

  def randomHex: CompiledDefinition = noParamDefinition("randomHex", CompiledFilter.constE {
    Eff.send[TaskParallel, CompiledFilterStack, List[JSON]](Task.eval(JSON.str(List.fill(6) {
      java.lang.Integer.toHexString(scala.util.Random.nextInt(256))
    }.mkString) :: Nil).parallel)
  })

  override def all(implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] =
    Right(
      Vector(googleAuth, launchAuth, randomHex, httpDelete, httpGet, httpPost, httpPatch, httpPut, nowRFC3339, formatDatetimeFriendly)
    )
}
