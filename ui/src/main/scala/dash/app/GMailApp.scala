package dash.app

import java.util.concurrent.TimeUnit

import com.thoughtworks.each.Monadic._
import qq.ajax._
import dash.models.{ExpandableContentModel, TitledContentModel}
import dash.{DomStorage, LoggerFactory, StorageProgram, identify}
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
import scala.scalajs.js.{Array, Dynamic, JSON, UndefOr}
import scalaz.\/
import scalaz.syntax.monad._
import scalaz.syntax.traverse._
import dash.Util._

object GMailApp extends DashApp {

  import GMailBindings._

  def threadToTitledContentModel(jsv: js.Any): TitledContentModel = {
    val json = jsv.asInstanceOf[Dynamic].messages.asInstanceOf[js.Array[js.Any]](0).asInstanceOf[Dynamic]
    val subject = json.payload.headers.asInstanceOf[js.Array[js.Any]](0).asInstanceOf[Dynamic].value.asInstanceOf[String]
    val snippet = json.snippet.asInstanceOf[String]
    TitledContentModel(subject, None, snippet)
  }

  def fetchMail: Task[Observable[IndexedSeq[ExpandableContentModel]]] = monadic[Task] {

    implicit val logger = LoggerFactory.getLogger("GmailApp")

    val app =
      """
def headers: {
  Authorization: "Bearer " + googleAuth
};
def listParams: {
  maxResults: 10,
  q: "is:unread"
};
def getParams: {
  format: "metadata",
  metadataHeaders: "Subject",
  fields: "messages(payload/headers,snippet)"
};
def threadList:
  httpGet("https://www.googleapis.com/gmail/v1/users/me/threads"; listParams; ""; headers) | .threads | .[];
def threadDetails:
  threadList | .id | httpGet("https://www.googleapis.com/gmail/v1/users/me/threads/" + .; getParams; ""; headers);
threadDetails
      """

    val programInStorage = StorageProgram.runProgram(DomStorage.Local, getCompiledProgram(app)).flatMap(_.valueOrThrow).each(js.Array[Any]())
    val titledContentModels =
      programInStorage.map(_.map(a => threadToTitledContentModel(a.asInstanceOf[js.Any])))

    val expandableContentModels = titledContentModels.map(c => IndexedSeq(ExpandableContentModel("Gmail", None, c)))

    Observable.fromTask(expandableContentModels)

  }
}
