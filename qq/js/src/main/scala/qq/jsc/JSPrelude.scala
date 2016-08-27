package qq.jsc

import java.util.regex.Pattern

import com.thoughtworks.each.Monadic._
import monix.eval.Task
import monix.scalaz._
import qq._
import qq.ajax.{Ajax, AjaxMethod}

import scala.scalajs.js
import scalaz.std.list._
import scala.concurrent.duration._
import scalaz.syntax.either._
import scalaz.syntax.traverse._

object JSPrelude extends PlatformPrelude[Any] {

  import CompiledDefinition.noParamDefinition

  override def length: CompiledDefinition[Any] =
    noParamDefinition(
      "length", {
        case arr: js.Array[js.Any@unchecked] => Task.now(arr.length :: Nil)
        case str: String => Task.now(str.length :: Nil)
        case obj: js.Object => Task.now(obj.asInstanceOf[js.Dictionary[js.Any]].toArray.length :: Nil)
        case null => Task.now(0 :: Nil)
        case k => Task.raiseError(QQRuntimeException("Tried to get length of " + String.valueOf(k)))
      }
    )

  override def replaceAll: CompiledDefinition[Any] =
    CompiledDefinition[Any](name = "replaceAll", numParams = 2,
      body = {
        case (regexFilter :: replacementFilter :: Nil) => {
          (jsv: Any) =>
            monadic[Task] {
              val regexes: List[Pattern] = regexFilter(jsv).each.traverse[Task, Pattern] {
                case string: String => Task.now(Pattern.compile(string))
                case j => Task.raiseError(NotARegex(String.valueOf(j)))
              }.each
              val replacements: List[String] = replacementFilter(jsv).each.traverse[Task, String] {
                case string: String => Task.now(string)
                case j => Task.raiseError(QQRuntimeException("can't replace with " + String.valueOf(j)))
              }.each
              val valueRegexReplacementList = (regexes, replacements).zipped.map { (regex, replacement) =>
                jsv match {
                  case string: String =>
                    Task.now(regex.matcher(string).replaceAll(replacement): Any)
                  case j =>
                    Task.raiseError(QQRuntimeException("can't replace " + String.valueOf(j)))
                }
              }.sequence[Task, Any].each
              valueRegexReplacementList
            }
        }.right[QQCompilationException]
      }
    )

  override def keys: CompiledDefinition[Any] =
    noParamDefinition(
      "keys", {
        case obj: js.Object => Task.now(js.Object.keys(obj) :: Nil)
        case k => Task.raiseError(QQRuntimeException("Tried to get keys of " + String.valueOf(k)))
      }
    )

  override def arrays: CompiledDefinition[Any] =
    noParamDefinition(
      "arrays", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case _ => Task.now(Nil)
      })

  override def objects: CompiledDefinition[Any] =
    noParamDefinition(
      "objects", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case _ => Task.now(Nil)
      })

  override def iterables: CompiledDefinition[Any] =
    noParamDefinition(
      "iterables", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case _ => Task.now(Nil)
      })

  override def booleans: CompiledDefinition[Any] =
    noParamDefinition(
      "booleans", {
        case null => Task.now(Nil)
        case bool: java.lang.Boolean => Task.now(bool :: Nil)
        case _ => Task.now(Nil)
      })

  override def numbers: CompiledDefinition[Any] =
    noParamDefinition(
      "numbers", {
        case null => Task.now(Nil)
        case num: java.lang.Double => Task.now(num :: Nil)
        case _ => Task.now(Nil)
      })

  override def strings: CompiledDefinition[Any] =
    noParamDefinition(
      "strings", {
        case null => Task.now(Nil)
        case str: String => Task.now(str :: Nil)
        case _ => Task.now(Nil)
      })

  override def nulls: CompiledDefinition[Any] =
    noParamDefinition(
      "nulls", {
        case null => Task.now(null :: Nil)
        case _ => Task.now(Nil)
      })

  override def values: CompiledDefinition[Any] =
    noParamDefinition(
      "values", {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: CompiledDefinition[Any] =
    noParamDefinition(
      "scalars", {
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  private def makeAjaxDefinition(name: String, ajaxMethod: AjaxMethod) = CompiledDefinition[Any](name, 4, {
    case List(urlFilter, queryParamsFilter, dataFilter, headersFilter) => { (jsv: Any) =>
      monadic[Task] {
        val url = (urlFilter(jsv).each.head match {
          case s: String => Task.now(s)
          case k => Task.raiseError(QQRuntimeException(String.valueOf(k) + " is not a URL"))
        }).each
        val queryParams = (queryParamsFilter(jsv).each.head match {
          case o: js.Object => Task.now(o)
          case k => Task.raiseError(QQRuntimeException(String.valueOf(k) + " is not query params/data"))
        }).each.asInstanceOf[js.Dictionary[js.Any]]
        val data = (dataFilter(jsv).each.head match {
          case s: String => Task.now(s)
          case k => Task.raiseError(QQRuntimeException(String.valueOf(k) + " is not usable as POST data"))
        }).each
        val headers = (headersFilter(jsv).each.head match {
          case o: js.Object => Task.now(o)
          case k => Task.raiseError(QQRuntimeException(String.valueOf(k) + " is not headers"))
        }).each.asInstanceOf[js.Dictionary[String]]
        val ajax = (if (ajaxMethod == AjaxMethod.POST) {
          Ajax.post(url, data, queryParams.toMap, headers.toMap)(Ajax.Timeout(1000.millis))
        } else {
          Ajax.apply(AjaxMethod.asString(ajaxMethod), url, null, queryParams.toMap, headers.toMap, false, "")(Ajax.Timeout(1000.millis))
        }).each
        List(Json.stringToJs(ajax.responseText).fold(Task.raiseError, Task.now).each)
      }
    }.right
  })

  override def httpDelete: CompiledDefinition[Any] = makeAjaxDefinition("httpDelete", AjaxMethod.DELETE)

  override def httpGet: CompiledDefinition[Any] = makeAjaxDefinition("httpGet", AjaxMethod.GET)

  override def httpPost: CompiledDefinition[Any] = makeAjaxDefinition("httpGet", AjaxMethod.POST)

  override def httpPatch: CompiledDefinition[Any] = makeAjaxDefinition("httpGet", AjaxMethod.PATCH)

  override def httpPut: CompiledDefinition[Any] = makeAjaxDefinition("httpPut", AjaxMethod.PUT)

}

