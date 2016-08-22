package dash.app

import java.util.concurrent.TimeUnit

import com.thoughtworks.each.Monadic._
import dash.ajax._
import dash.models.{ExpandableContentModel, TitledContentModel}
import dash.{LoggerFactory, identify}
import monix.eval.Task
import monix.reactive.Observable
import monix.scalaz._
import qq.Unsafe
import qq.jsc.Json
import shapeless._
import shapeless.record._
import shapeless.syntax.singleton._

import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration.Duration
import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.JSON
import scalaz.\/
import scalaz.syntax.monad._
import scalaz.syntax.traverse._

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
      Record.`"format" -> UndefOr[String], "metadataHeaders" -> UndefOr[String], "fields" -> UndefOr[String]`.T


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
      Ajax.boundConstantPath(Messages.list,
        "includeSpamTrash" ->> js.undefined ::
          "labelIds" ->> js.undefined ::
          "maxResults" ->> (10: UndefOr[Int]) ::
          "pageToken" ->> js.undefined ::
          "q" ->> ("is:unread": UndefOr[String]) ::
          HNil,
        defaultHeaders
      ).map(r => Json.stringToJs(r.responseText))
    val ids = unreadMessagesResponse.map(_.map(_.asInstanceOf[js.Dynamic].messages.asInstanceOf[js.Array[js.Dynamic]].map(_.id.asInstanceOf[String])))

    import qq.Platform.Js.Unsafe._

    val messages = ids.flatMap(_.traverse[Task, upickle.Invalid.Json, js.WrappedArray[upickle.Invalid.Json \/ js.Any]](idArr =>
      Task.gatherUnordered(
        new js.WrappedArray(idArr).map { id =>
          Ajax.bound(Messages.get,
            "format" ->> ("metadata": UndefOr[String]) ::
              "metadataHeaders" ->> ("Subject": UndefOr[String]) ::
              "fields" ->> ("snippet,payload(headers)": UndefOr[String]) ::
              HNil,
            defaultHeaders,
            id :: HNil
          ).map { resp => Json.stringToJs(resp.responseText) }
        }
      )
    ))

    val d =
      Observable.fromTask(messages)
        .map(_.flatMap(Unsafe.builderTraverse[js.WrappedArray].sequence(_)))
        .map(_.valueOr(_ => js.WrappedArray.empty)
          .map { (jsv: js.Any) =>
            val json = jsv.asInstanceOf[js.Dynamic]
            val subject = json.payload.headers.asInstanceOf[js.Array[js.Any]](0).asInstanceOf[js.Dynamic].value.asInstanceOf[String]
            val snippet = json.snippet.asInstanceOf[String]
            val ret = TitledContentModel(subject, None, snippet)
            ret
          }
          .toList
        )

    val seq = d.map(c => IndexedSeq(ExpandableContentModel("Gmail", None, c)))

    seq

  }

}
