package dash

import com.thoughtworks.each.Monadic._
import dash.models.{ExpandableContentModel, TitledContentModel}
import monix.eval.Task
import monix.reactive.Observable
import monix.scalaz._
import org.scalajs.dom.XMLHttpRequest
import upickle.{Js, json}

import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration._
import scalaz.std.list._
import scalaz.syntax.traverse._

object GMailApp {

  def fetchMail: Task[Observable[IndexedSeq[ExpandableContentModel]]] = monadic[Task] {


    ???
  }

}
