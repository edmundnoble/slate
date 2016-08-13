package dash.app

import com.thoughtworks.each.Monadic._
import dash.chrome.{GetAuthTokenOptions, RemoveCachedAuthTokenOptions}
import dash.models.ExpandableContentModel
import monix.eval.Task
import monix.reactive.Observable
import monix.scalaz._

import scala.collection.immutable.IndexedSeq

object GMailApp {

  def fetchMail: Task[Observable[IndexedSeq[ExpandableContentModel]]] = monadic[Task] {

    //    https://accounts.google.com/o/oauth2/v2/auth
    //    Ajax.get
    //    response_type

    val authToken = dash.chrome.identity.getAuthToken(new GetAuthTokenOptions(interactive = true)).each
    dash.chrome.identity.removeCachedAuthToken(new RemoveCachedAuthTokenOptions(token = authToken.get)).each
    val authHeader = "Authorization" -> s"Bearer $authToken"

    println(s"authHeader: $authHeader")

    ???

  }

}
