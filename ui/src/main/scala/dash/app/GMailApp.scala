package dash.app

import java.util.concurrent.TimeUnit

import com.thoughtworks.each.Monadic._
import dash.{Ajax, Logger, LoggerFactory, identify}
import dash.chrome.{GetAuthTokenOptions, RemoveCachedAuthTokenOptions}
import dash.models.ExpandableContentModel
import monix.eval.Task
import monix.reactive.Observable
import monix.scalaz._

import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration.Duration
import scala.scalajs.js
import scala.scalajs.js.JSON

object GMailApp {

  def fetchMail: Task[Observable[IndexedSeq[ExpandableContentModel]]] = monadic[Task] {

    implicit val ajaxTimeout = Ajax.Timeout(Duration(4000, TimeUnit.MILLISECONDS))

    implicit val logger = LoggerFactory.getLogger("GmailApp")

    //    https://accounts.google.com/o/oauth2/v2/auth
    //    Ajax.get
    //    response_type

    val authToken = identify.getAuthToken(interactive = true).each
//    val () = identify.removeCachedAuthToken(token = authToken).each
    val authHeader = "Authorization" -> ("Bearer " + authToken)
    val getThreadsUrl = "https://www.googleapis.com/gmail/v1/users/me/threads"
    val threadsResponse = JSON.parse(Ajax.get(getThreadsUrl, headers = Map(authHeader), data = Map("prettyPrint" -> "false")).each.responseText)
    logger.info("threadsResponse: " + JSON.stringify(threadsResponse, null: js.Array[js.Any], space = scalajs.js.Any.fromInt(2)))

    ???

  }

}
