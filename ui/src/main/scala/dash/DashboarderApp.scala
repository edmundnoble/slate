package dash

import java.util.regex.Pattern

import com.thoughtworks.each.Monadic._
import dash.DashboardPage.{Filter, Issue, SearchResult}
import japgolly.scalajs.react.{Addons, ReactDOM}
import monix.eval.{Callback, Task}
import monix.reactive.Observable
import org.scalajs.dom.XMLHttpRequest
import org.scalajs.dom.raw.Element
import qq.Util._
import upickle.{Js, json}

import scala.concurrent.duration._
import scala.scalajs.js.{Date, JSON}
import scala.scalajs.js.annotation.JSExport
import scala.util.Try
import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.vector._

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  import scala.scalajs.js

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  val loginDelay: FiniteDuration = 50.millis

  @JSExport
  def main(): Unit = {
    val fetchSearchResults = monadic[Task] {

      implicit val ajaxTimeout = Ajax.Timeout(4000.millis)

      val searchRequests = for {
        favoriteFilterResponse <- Observable.fromTask(
          Ajax.get(url = "https://jira.atlassian.net/rest/api/2/filter/favourite", headers = Creds.authData)
        )
        favoriteFilters: List[Filter] = json.read(favoriteFilterResponse.responseText).arr.map { r =>
          Filter(r.obj("self").str, r.obj("name").str, r.obj("owner").obj("name").str, r.obj("jql").str, r.obj("viewUrl").str)
        }(collection.breakOut)
        filterRequests <- (favoriteFilters: List[Filter]).traverse[Observable, XMLHttpRequest] { filter =>
          Observable.fromTask(
            Ajax.post(url = s"https://jira.atlassian.net/rest/api/2/search/",
              data = json.write(Js.Obj("jql" -> Js.Str(filter.jql), "maxResults" -> Js.Num(10))),
              headers = Creds.authData ++ Map("Content-Type" -> "application/json"))
          )
        }
      } yield (favoriteFilters, filterRequests)

      val compiledQQProgram = qq.Runner.parseAndCompile(qq.UpickleRuntime,
        """.issues.[] | {
          |  url: .self,
          |  summary: .fields.summary,
          |  key,
          |  project: .fields.project.name,
          |  description: (.fields.description | replaceAll("\n+\\s*"; " ↪ ")),
          |  status: .fields.status.name
          |}""".stripMargin
      ).fold(Task.raiseError, Task.now).each
      val searchResultStream = searchRequests.flatMap {
        case (filter, responses) =>
          responses.traverse[Observable, List[Issue]] { r =>
            val results = compiledQQProgram(upickle.json read r.responseText)
            Observable.fromTask(results map (_ flatMap (Issue.pkl.read.lift(_))))
          }.strengthL(filter)
      }

      searchResultStream.map { searchResults =>
        searchResults.zipped.map(SearchResult(_, _))(collection.breakOut)
      }
    }

    {
      import monix.execution.Scheduler.Implicits.global

      val callback = new Callback[Observable[IndexedSeq[SearchResult]]] {
        override def onSuccess(results: Observable[IndexedSeq[SearchResult]]): Unit = {
          val searchPage =
            DashboardPage.makeSearchPage(results)
          val container: Element = org.scalajs.dom.document.body.children.namedItem("container")
          if (!js.isUndefined(Addons.Perf)) {
            logger.info("Starting perf")
            Addons.Perf.start()
            logger.info("Rendering DOM")
          }
          ReactDOM.render(searchPage, container)
          if (!js.isUndefined(Addons.Perf)) {
            logger.info("Stopping perf")
            Addons.Perf.stop()
            logger.info("Printing wasted")
            logger.info(Addons.Perf.printWasted(Addons.Perf.getLastMeasurements()).toString)
          }
        }

        override def onError(error: Throwable): Unit = {
          logger.error(s"EXCEPTION INTERRUPTED MAIN:\n$error\n${error.getStackTrace.mkString("\n")}")
        }
      }

      val _ = fetchSearchResults.runAsync(callback)
    }
  }

}
