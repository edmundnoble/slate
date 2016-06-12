package dash

import java.nio.ByteBuffer

import monix.eval.Task
import monix.execution.Cancelable
import org.scalajs.dom
import org.scalajs.dom.FormData
import org.scalajs.dom.raw.Blob

import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._

/**
  * Thrown when `Ajax.get` or `Ajax.post` receives a non-20X response code.
  * Contains the XMLHttpRequest that resulted in that response
  */
case class AjaxException(xhr: dom.XMLHttpRequest) extends Exception {
  def isTimeout = xhr.status == 0 && xhr.readyState == 4
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
  sealed trait InputData extends js.Any

  object InputData {
    implicit def str2ajax(s: String): InputData = s.asInstanceOf[InputData]

    implicit def arrayBufferView2ajax(b: ArrayBufferView): InputData = b.asInstanceOf[InputData]

    implicit def blob2ajax(b: Blob): InputData = b.asInstanceOf[InputData]

    implicit def formdata2ajax(b: FormData): InputData = b.asInstanceOf[InputData]

    implicit def byteBuffer2ajax(data: ByteBuffer): InputData = {
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

  def get(url: String,
          data: InputData = null,
          timeout: Int = 0,
          headers: Map[String, String] = Map.empty,
          withCredentials: Boolean = false,
          responseType: String = "") = {
    apply("GET", url, data, timeout, headers, withCredentials, responseType)
  }

  def post(url: String,
           data: InputData = null,
           timeout: Int = 0,
           headers: Map[String, String] = Map.empty,
           withCredentials: Boolean = false,
           responseType: String = "") = {
    apply("POST", url, data, timeout, headers, withCredentials, responseType)
  }

  def put(url: String,
          data: InputData = null,
          timeout: Int = 0,
          headers: Map[String, String] = Map.empty,
          withCredentials: Boolean = false,
          responseType: String = "") = {
    apply("PUT", url, data, timeout, headers, withCredentials, responseType)
  }

  def delete(url: String,
             data: InputData = null,
             timeout: Int = 0,
             headers: Map[String, String] = Map.empty,
             withCredentials: Boolean = false,
             responseType: String = "") = {
    apply("DELETE", url, data, timeout, headers, withCredentials, responseType)
  }

  def apply(method: String,
            url: String,
            data: InputData,
            timeout: Int,
            headers: Map[String, String],
            withCredentials: Boolean,
            responseType: String): Task[dom.XMLHttpRequest] = {
    Task.create[dom.XMLHttpRequest] { (_, callback) =>
      val req = new dom.XMLHttpRequest()

      req.onreadystatechange = { (e: dom.Event) =>
        if (req.readyState.toInt == 4) {
          if ((req.status >= 200 && req.status < 300) || req.status == 304)
            callback.onSuccess(req)
          else
            callback.onError(AjaxException(req))
        }
      }
      req.open(method, url)
      req.responseType = responseType
      req.timeout = timeout
      req.withCredentials = withCredentials
      headers.foreach(x => req.setRequestHeader(x._1, x._2))
      if (data == null)
        req.send()
      else
        req.send(data)

      Cancelable(() => req.abort())
    }
  }
}