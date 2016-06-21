package dash

import java.util.regex.Pattern

import com.thoughtworks.each.Monadic._
import dash.DashboardPage.{Filter, Issue, SearchResult}
import japgolly.scalajs.react.{Addons, ReactDOM}
import monix.eval.{Callback, Task}
import org.scalajs.dom.XMLHttpRequest
import org.scalajs.dom.raw.Element
import qq.Util._
import upickle.{Js, json}

import scala.concurrent.duration._
import scala.scalajs.js.Date
import scala.scalajs.js.annotation.JSExport
import scala.util.Try
import scalaz.syntax.traverse._
import scalaz.std.list._

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  import scala.scalajs.js

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  val loginDelay: FiniteDuration = 50.millis

  @JSExport
  def main(): Unit = {
    val longRegex = Pattern.compile("\n+\\s*", Pattern.MULTILINE)

    val fetchSearchResults = monadic[Task] {

      implicit val ajaxTimeout = Ajax.Timeout(4000.millis)

      val favoriteFilterResponse = Ajax.get(url = "https://auviknetworks.atlassian.net/rest/api/2/filter/favourite", headers = Creds.authData).each
      logger.info(s"favoriteFilterResponse ${favoriteFilterResponse.status} ${favoriteFilterResponse.statusText}: ${favoriteFilterResponse.responseText}")

      val favoriteFilters: List[Filter] = json.read(favoriteFilterResponse.responseText).arr.map { r =>
        Filter(r.obj("self").str, r.obj("name").str, r.obj("owner").obj("name").str, r.obj("jql").str, r.obj("viewUrl").str)
      }(collection.breakOut)

      val searchRequests = favoriteFilters.traverse[Task, XMLHttpRequest] { filter =>
        Ajax.post(url = s"https://auviknetworks.atlassian.net/rest/api/2/search/",
          data = json.write(Js.Obj("jql" -> Js.Str(filter.jql), "maxResults" -> Js.Num(10))),
          headers = Creds.authData ++ Map("Content-Type" -> "application/json"))
      }.each

      val searchResults = searchRequests.map(r => json.read(r.responseText).obj("issues").arr.map { issueObj =>
        val fields = issueObj("fields").obj
        val url = issueObj("self").str
        val summary = fields("summary").str
        val key = issueObj("key").str
        val project = fields("project").obj("name").str
        val status = fields("status").obj("name").str
        val shortStatus = new String(status.split(" ").filter(_.nonEmpty).map(s => Character.toUpperCase(s.charAt(0))).toArray)
        val assignee = Try(fields("assignee").obj("name").str).toOption
        val reporter = Try(fields("reporter").obj("name").str).toOption
        val description = longRegex.matcher(fields("description").str).replaceAll(" ↪ ")
        Issue(
          url, summary, key, project, assignee, reporter, shortStatus, description,
          new Date(fields("created").str).getTime(),
          new Date(fields("updated").str).getTime()
        )
      })

      (favoriteFilters, searchResults).zipped.map(SearchResult(_, _))(collection.breakOut)
    }

    val callback = new Callback[IndexedSeq[SearchResult]] {
      override def onSuccess(results: IndexedSeq[SearchResult]): Unit = {
        val searchPage =
          DashboardPage.makeSearchPage(results)
        logger.info(s"Rendered ${results.length} filters!")
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
        error match {
          case ex: Exception =>
            logger.error("EXCEPTION INTERRUPTED MAIN:", ex)
          case _ =>
        }
      }
    }

    {
      import monix.execution.Scheduler.Implicits.global
      fetchSearchResults.runAsync(callback)
    }
  }

}
