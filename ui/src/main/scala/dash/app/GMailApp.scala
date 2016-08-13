package dash.app

import com.thoughtworks.each.Monadic._
import dash.models.ExpandableContentModel
import monix.eval.Task
import monix.reactive.Observable
import monix.scalaz._

import scala.collection.immutable.IndexedSeq

object GMailApp {

  def fetchMail: Task[Observable[IndexedSeq[ExpandableContentModel]]] = monadic[Task] {

//    https://accounts.google.com/o/oauth2/v2/auth
    Ajax.get
    response_type

    ???
  }

}
