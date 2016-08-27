package dash.app

import java.util.concurrent.TimeUnit

import com.thoughtworks.each.Monadic._
import qq.ajax._
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
import scala.scalajs.js.{Any, Array, Dynamic, JSON, UndefOr}
import scalaz.\/
import scalaz.syntax.monad._
import scalaz.syntax.traverse._

object GMailApp {

  import GMailBindings._

  def threadToTitledContentModel(jsv: Any): TitledContentModel = {
    val json = jsv.asInstanceOf[Dynamic].messages.asInstanceOf[js.Array[Any]](0).asInstanceOf[Dynamic]
    val subject = json.payload.headers.asInstanceOf[js.Array[Any]](0).asInstanceOf[Dynamic].value.asInstanceOf[String]
    val snippet = json.snippet.asInstanceOf[String]
    TitledContentModel(subject, None, snippet)
  }

  def fetchMail: Task[Observable[IndexedSeq[ExpandableContentModel]]] = monadic[Task] {

    implicit val ajaxTimeout = Ajax.Timeout(Duration(4000, TimeUnit.MILLISECONDS))

    implicit val logger = LoggerFactory.getLogger("GmailApp")

    //    https://accounts.google.com/o/oauth2/v2/auth
    //    Ajax.get
    //    response_type

    import qq.Platform.Js.Unsafe._

    val authToken = identify.getAuthToken(interactive = false).each
    //    val () = identify.removeCachedAuthToken(token = authToken).each
    val authHeader = "Authorization" ->> ("Bearer " + authToken)
    val defaultHeaders =
      authHeader ::
        "Cache-Control" ->> "no-cache" ::
        HNil
    val unreadThreadsResponse =
      Ajax.boundConstantPath(Threads.list)(
        queryParams = "includeSpamTrash" ->> js.undefined ::
          "labelIds" ->> js.undefined ::
          "maxResults" ->> (10: UndefOr[Int]) ::
          "pageToken" ->> js.undefined ::
          "q" ->> ("is:unread": UndefOr[String]) ::
          HNil,
        headers = defaultHeaders
      ).map(r => Json.stringToJs(r.responseText))
    val ids = unreadThreadsResponse.map(_.map { any =>
      any.asInstanceOf[Dynamic].threads.asInstanceOf[Array[Dynamic]].map(_.id.asInstanceOf[String])
    })

    val threads = Observable
      .fromTask(ids)
      .flatMap {
        _.traverse[Observable, upickle.Invalid.Json, Seq[upickle.Invalid.Json \/ js.Any]](idArr =>
          Observable.combineLatestList(
            new js.WrappedArray(idArr).map { id =>
              val getMessage = Ajax.bound(Threads.get)(
                data = "format" ->> ("metadata": UndefOr[String]) ::
                  "metadataHeaders" ->> ("Subject": UndefOr[String]) ::
                  "fields" ->> ("messages(payload/headers,snippet)": UndefOr[String]) ::
                  HNil,
                headers = defaultHeaders,
                pathArgs = ("id" ->> id) :: HNil
              ).map { resp => Json.stringToJs(resp.responseText) }
              Observable.fromTask(getMessage)
            }: _*
          )
        )
      }

    val titledContentModels =
      threads.map {
        _.flatMap(Unsafe.builderTraverse[Seq].sequence(_))
          .valueOr(_ => js.WrappedArray.empty).map(threadToTitledContentModel).toList
      }

    val expandableContentModels = titledContentModels.map(c => IndexedSeq(ExpandableContentModel("Gmail", None, c)))

    expandableContentModels

  }
}
