package dash.app

import java.util.concurrent.TimeUnit

import com.thoughtworks.each.Monadic._
import dash.ajax.SingleBinding
import dash.ajax.{Ajax, Bindings}
import dash.models.ExpandableContentModel
import dash.{LoggerFactory, identify}
import monix.eval.Task
import monix.reactive.Observable
import monix.scalaz._
import qq.jsc.Json
import shapeless._
import shapeless.record._
import shapeless.syntax.singleton._

import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration.Duration
import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.JSON

object GMailApp {

  object Messages {
    val url = "https://www.googleapis.com/gmail/v1/users/me/messages"
    val get = new SingleBinding[GetData, AuthHeaders](url, dash.ajax.GET)

    type AuthHeaders =
      Record.`"Authorization" -> String, "Cache-Control" -> String`.T

    type GetData =
      Record.`"includeSpamTrash" -> UndefOr[Boolean], "labelIds" -> UndefOr[String], "maxResults" -> UndefOr[Int], "pageToken" -> UndefOr[Int], "q" -> UndefOr[String]`.T
  }

  def fetchMail: Task[Observable[IndexedSeq[ExpandableContentModel]]] = monadic[Task] {

    implicit val ajaxTimeout = Ajax.Timeout(Duration(4000, TimeUnit.MILLISECONDS))

    implicit val logger = LoggerFactory.getLogger("GmailApp")

    //    https://accounts.google.com/o/oauth2/v2/auth
    //    Ajax.get
    //    response_type

    val authToken = identify.getAuthToken(interactive = true).each
    //    val () = identify.removeCachedAuthToken(token = authToken).each
    val authHeader = "Authorization" ->> ("Bearer " + authToken)
    val unreadMessagesResponse =
      Json.stringToJs(
        Ajax.bound(Messages.get)(
          data =
            "includeSpamTrash" ->> js.undefined ::
              "labelIds" ->> js.undefined ::
              "maxResults" ->> (10: UndefOr[Int]) ::
              "pageToken" ->> js.undefined ::
              "q" ->> ("is:unread": UndefOr[String]) ::
              HNil,
          headers =
            authHeader ::
              "Cache-Control" ->> "no-cache" ::
              HNil
        )
          .each.responseText
      )
    logger.info("MessagesResponse: " + unreadMessagesResponse.map(JSON.stringify(_, null: js.Array[js.Any], space = scalajs.js.Any.fromInt(2))))

    Observable.now(IndexedSeq.empty[ExpandableContentModel])

  }

}
