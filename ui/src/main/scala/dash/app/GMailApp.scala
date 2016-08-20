package dash.app

import java.util.concurrent.TimeUnit

import com.thoughtworks.each.Monadic._
import dash.ajax._
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

    import PathSegment.DSL._

    val listPath = "https://www.googleapis.com/gmail/v1/users/me/messages" :/: PathEnding
    val list =
      Binding[StringPathSegment[PathEnding.type], ListData, AuthHeaders](listPath, AjaxMethod.GET)

    val getPath = "https://www.googleapis.com/gmail/v1/users/me/messages" :/: StringTag :/: PathEnding
    val get = Binding[StringPathSegment[GenericPathSegment[String, PathEnding.type]], GetData, AuthHeaders](getPath, AjaxMethod.GET)

    type AuthHeaders =
      Record.`"Authorization" -> String, "Cache-Control" -> String`.T

    type GetData =
      Record.`"format" -> UndefOr[String], "metadataHeaders" -> UndefOr[String]`.T

    type ListData =
      Record.`"includeSpamTrash" -> UndefOr[Boolean], "labelIds" -> UndefOr[String], "maxResults" -> UndefOr[Int], "pageToken" -> UndefOr[Int], "q" -> UndefOr[String]`.T
  }

  def fetchMail: Task[Observable[IndexedSeq[ExpandableContentModel]]] = monadic[Task] {

    implicit val ajaxTimeout = Ajax.Timeout(Duration(4000, TimeUnit.MILLISECONDS))

    implicit val logger = LoggerFactory.getLogger("GmailApp")

    //    https://accounts.google.com/o/oauth2/v2/auth
    //    Ajax.get
    //    response_type

    val authToken = identify.getAuthToken(interactive = false).each
    //    val () = identify.removeCachedAuthToken(token = authToken).each
    val authHeader = "Authorization" ->> ("Bearer " + authToken)
    val defaultHeaders =
      authHeader ::
        "Cache-Control" ->> "no-cache" ::
        HNil
    val unreadMessagesResponse =
      Json.stringToJs(
        Ajax.boundConstantPath(Messages.list,
          "includeSpamTrash" ->> js.undefined ::
            "labelIds" ->> js.undefined ::
            "maxResults" ->> (10: UndefOr[Int]) ::
            "pageToken" ->> js.undefined ::
            "q" ->> ("is:unread": UndefOr[String]) ::
            HNil,
          defaultHeaders
        ).each.responseText
      )
    val ids = unreadMessagesResponse.map(_.asInstanceOf[js.Dynamic].messages.asInstanceOf[js.Array[js.Dynamic]].map(_.id.asInstanceOf[String]))
    val messages = ids.traverse(idArr =>
      Task.gatherUnordered(
        new js.WrappedArray(idArr).map { id =>
          Ajax.bound(Messages.get,
            "format" ->> ("metadata": UndefOr[String]) ::
              "metadataHeaders" ->> js.undefined ::
              HNil,
            defaultHeaders,
            id :: HNil
          ).map(_.responseText)
        }
      )
    ).each
//    logger.info("MessagesResponse: " + messages.map(Json.jsToString(_, space = 2)))

    Observable.now(IndexedSeq.empty[ExpandableContentModel])

  }

}
