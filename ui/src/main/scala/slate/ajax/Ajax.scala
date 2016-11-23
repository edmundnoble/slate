package slate
package ajax

import java.nio.ByteBuffer

import cats.data.NonEmptyList
import monix.eval.Task
import monix.execution.Cancelable
import org.scalajs.dom
import org.scalajs.dom.raw.Blob
import org.scalajs.dom.{Event, FormData, XMLHttpRequest}
import qq.cc.{QQRuntimeError, QQRuntimeException}
import qq.data.JSON
import qq.data.JSON.ObjList

import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._

object AjaxException {
  def errorCodeToMessage: Int => Option[String] = Map[Int, String](
    400 -> "(bad request) the server rejected the request as malformed"
    , 401 -> "(forbidden) you do not have permission from the server to access the document"
    , 404 -> "(not found) the server could not find the document requested"
    , 405 -> "(method not allowed) the server does not allow that method on that resource"
    , 406 -> "(not acceptable) the server is not capable of generating content according to the accept headers in the request"
    , 500 -> "(internal server error) for some reason, the server is not capable of fulfilling the request"
    , 501 -> "(not implemented) at least one of the request method/URL are unrecognized, but they may be available in future"
    , 502 -> "(bad gateway) the server was acting as a gateway or proxy and received an invalid response from the upstream server"
    , 503 -> "(service unavailable) the server is currently unavailable; generally temporary"
  ).lift
}

/**
  * Thrown when `Ajax.get` or `Ajax.post` receives a non-20X response code.
  * Contains the XMLHttpRequest that resulted in that response
  */
case class AjaxException(xhr: dom.XMLHttpRequest, url: String)
  extends QQRuntimeError(if (xhr.status == 0 && xhr.readyState == 4) s"HTTP timeout from $url" else
    s"HTTP error ${xhr.status} from $url: ${AjaxException.errorCodeToMessage(xhr.status).getOrElse("unknown error code")}.") {
  def isTimeout: Boolean = xhr.status == 0 && xhr.readyState == 4
}

/**
  * Wraps an XMLHttpRequest to provide an easy one-line way of making
  * an Ajax call, returning a Future.
  */
object Ajax {

  /**
    * Supported data formats for Ajax are implicitly converted to InputData
    */
  @js.native
  sealed trait PostData extends js.Any

  case class Timeout(value: Duration)

  object PostData {
    implicit def str2ajax(s: String): PostData = s.asInstanceOf[PostData]

    implicit def arrayBufferView2ajax(b: ArrayBufferView): PostData = b.asInstanceOf[PostData]

    implicit def blob2ajax(b: Blob): PostData = b.asInstanceOf[PostData]

    implicit def formdata2ajax(b: FormData): PostData = b.asInstanceOf[PostData]

    implicit def byteBuffer2ajax(data: ByteBuffer): PostData = {
      if (data.hasTypedArray()) {
        // get relevant part of the underlying typed array
        data.typedArray().subarray(data.position, data.limit)
      } else {
        // fall back to copying the data
        val tempBuffer = ByteBuffer.allocateDirect(data.remaining)
        val origPosition = data.position
        tempBuffer.put(data)
        data.position(origPosition)
        tempBuffer.typedArray()
      }
    }
  }

  @inline final def get(url: String,
                        data: PostData = null,
                        queryParams: ObjList = JSON.Obj(),
                        headers: Map[String, String] = Map.empty,
                        withCredentials: Boolean = false,
                        responseType: String = "")(implicit timeout: Timeout): Task[XMLHttpRequest] = {
    apply("GET", url, data, queryParams, headers, withCredentials, responseType)
  }

  @inline final def post(url: String,
                         data: PostData = null,
                         queryParams: ObjList = JSON.Obj(),
                         headers: Map[String, String] = Map.empty,
                         withCredentials: Boolean = false,
                         responseType: String = "")(implicit timeout: Timeout): Task[XMLHttpRequest] = {
    apply("POST", url, data, queryParams, headers, withCredentials, responseType)
  }

  @inline final def put(url: String,
                        data: PostData = null,
                        queryParams: ObjList = JSON.Obj(),
                        headers: Map[String, String] = Map.empty,
                        withCredentials: Boolean = false,
                        responseType: String = "")(implicit timeout: Timeout): Task[XMLHttpRequest] = {
    apply("PUT", url, data, queryParams, headers, withCredentials, responseType)
  }

  @inline final def delete(url: String,
                           data: PostData = null,
                           queryParams: ObjList = JSON.Obj(),
                           headers: Map[String, String] = Map.empty,
                           withCredentials: Boolean = false,
                           responseType: String = "")(implicit timeout: Timeout): Task[XMLHttpRequest] = {
    apply("DELETE", url, data, queryParams, headers, withCredentials, responseType)
  }

  def apply(method: String,
            url: String,
            data: PostData,
            queryParams: ObjList,
            headers: Map[String, String],
            withCredentials: Boolean,
            responseType: String)(implicit timeout: Timeout): Task[dom.XMLHttpRequest] = {
    Task.create[dom.XMLHttpRequest] { (_, callback) =>
      val req = new dom.XMLHttpRequest()

      req.onreadystatechange = { (_: Event) =>
        if (req.readyState == 4) {
          if ((req.status >= 200 && req.status < 300) || req.status == 304)
            callback.onSuccess(req)
          else
            callback.onError(QQRuntimeException(NonEmptyList(AjaxException(req, url), Nil)))
        }
      }

      val urlWithQuery = addQueryParams(url, queryParams)
      // TODO: Remove this shit when scala.js issue #2631 is fixed, crashes scoverage
      req.asInstanceOf[js.Dynamic].open(method, urlWithQuery)
      req.responseType = responseType
      req.asInstanceOf[js.Dynamic].timeout = if (timeout.value.isFinite()) timeout.value.toMillis.toInt else 0
      req.withCredentials = withCredentials
      headers.foreach((req.setRequestHeader _).tupled)
      if (data == null)
        req.asInstanceOf[js.Dynamic].send()
      else
        req.send(data)

      Cancelable(() => req.abort())
    }
  }

  def addQueryParams(url: String, queryParams: ObjList): String = {
    val queryString =
      queryParams.value.map { case (k, v) => k + "=" + JSON.renderBare(v) }.mkString("&")

    val urlWithQuery = if (queryParams.value.isEmpty) url else url + "?" + queryString
    urlWithQuery
  }

  def apply(method: AjaxMethod,
            url: String,
            data: PostData,
            queryParams: ObjList,
            headers: Map[String, String],
            withCredentials: Boolean,
            responseType: String)(implicit timeout: Timeout): Task[dom.XMLHttpRequest] =
    apply(AjaxMethod.asString(method), url, data, queryParams, headers, withCredentials, responseType)

}
