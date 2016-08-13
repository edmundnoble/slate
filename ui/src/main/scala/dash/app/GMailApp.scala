package dash.app

import java.util.concurrent.TimeUnit

import com.thoughtworks.each.Monadic._
import dash.{Ajax, LoggerFactory}
import dash.chrome.{GetAuthTokenOptions, RemoveCachedAuthTokenOptions}
import dash.models.ExpandableContentModel
import monix.eval.Task
import monix.reactive.Observable
import monix.scalaz._

import scala.collection.immutable.IndexedSeq
import dash.chrome.identity
import qq.jsc.Json

import scala.concurrent.duration.Duration

object GMailApp {

  def fetchMail: Task[Observable[IndexedSeq[ExpandableContentModel]]] = monadic[Task] {

    implicit val ajaxTimeout = Ajax.Timeout(Duration(4000, TimeUnit.MILLISECONDS))

    //    https://accounts.google.com/o/oauth2/v2/auth
    //    Ajax.get
    //    response_type

    val authToken = identity.getAuthToken(new GetAuthTokenOptions(interactive = true)).each
    val () = identity.removeCachedAuthToken(new RemoveCachedAuthTokenOptions(token = authToken.get)).each
    val authHeader = "Authorization" -> s"Bearer $authToken"
    val getThreadsUrl = "https://www.googleapis.com/gmail/v1/users/me/threads"
    val threadsResponse = Json.read(Ajax.get(getThreadsUrl, headers = Map(authHeader), data = Map("prettyPrint" -> "false")).each.responseText).obj("threads")
    LoggerFactory.getLogger("GmailApp").info(s"threadsResponse: ${Json.write(threadsResponse, indent = 2)}")

    ???

  }

}
