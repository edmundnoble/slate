package dash
package ajax

import java.nio.ByteBuffer

import cats.data.NonEmptyList
import monix.eval.Task
import monix.execution.Cancelable
import org.scalajs.dom
import org.scalajs.dom.{Event, FormData, XMLHttpRequest}
import org.scalajs.dom.raw.Blob
import qq.cc.{QQRuntimeError, QQRuntimeException}
import qq.data.JSON
import qq.data.JSON.ObjList

import scala.concurrent.duration._
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._

/**
  * Thrown when `Ajax.get` or `Ajax.post` receives a non-20X response code.
  * Contains the XMLHttpRequest that resulted in that response
  */
case class AjaxException(xhr: dom.XMLHttpRequest, url: String)
  extends QQRuntimeError("Ajax " + (if (xhr.status == 0 && xhr.readyState == 4) " timeout " else s" error ${xhr.status} ") + s" $url") {
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

      req.onreadystatechange = { (e: Event) =>
        if (req.readyState == 4) {
          if ((req.status >= 200 && req.status < 300) || req.status == 304)
            callback.onSuccess(req)
          else
            callback.onError(QQRuntimeException(NonEmptyList(AjaxException(req, url), Nil)))
        }
      }

      val urlWithQuery = addQueryParams(url, queryParams)
      req.open(method, urlWithQuery)
      req.responseType = responseType
      req.timeout = if (timeout.value.isFinite()) timeout.value.toMillis else 0
      req.withCredentials = withCredentials
      headers.foreach((req.setRequestHeader _).tupled)
      if (data == null)
        req.send()
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
