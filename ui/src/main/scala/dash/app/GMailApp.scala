package dash
package app

import dash.Util._
import dash.models.ExpandableContentModel
import monix.eval.Task
import monix.scalaz._
import qq.jsc.Json

import scala.collection.immutable.IndexedSeq
import scala.scalajs.js
import scala.scalajs.js.UndefOr
import qq.Platform.Rec._

object GMailApp extends DashApp {

  def fetchMail: Task[IndexedSeq[ExpandableContentModel]] = {

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
  [threadList | .id | httpGet("https://www.googleapis.com/gmail/v1/users/me/threads/" + .; getParams; ""; headers)];

def threadToContent: {
  title: "Gmail",
  content: [threadDetails | .[] | .messages.[0] | {
    title: .payload.headers.[0].value,
    content: .snippet
  }]
};

threadDetails | threadToContent
"""

    val programInStorage = StorageProgram.runProgram(DomStorage.Local, getCachedCompiledProgram(app)).flatMap(_.valueOrThrow).flatMap(_(js.Array[Any]()))

    programInStorage.map(_.flatMap(v => ExpandableContentModel.pkl.read.lift(Json.jsToUpickleRec(v))).toIndexedSeq)
  }
}
