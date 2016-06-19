package dash

import java.nio.charset.Charset
import java.util.regex.Pattern

import chrome.app.runtime.bindings.{LaunchData, Request}
import chrome.app.window.bindings.{BoundsSpecification, CreateWindowOptions}
import upickle.Js
import upickle.json
import dash.DashboardPage.{Filter, Issue, SearchResult}
import japgolly.scalajs.react.{Addons, ReactDOM}
import monix.eval.{Callback, Coeval, Task}
import org.scalajs.dom.raw.Element
import com.thoughtworks.each.Monadic._
import qq.Util._

import scalaz.std.scalaFuture._
import scalaz.syntax.monad._
import scalaz.std.vector._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.scalajs.js.Date
import scala.scalajs.js.annotation.JSExport
import scala.util.{Failure, Success, Try}

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  import scalacss.ScalaCssReact._
  import scalacss.Defaults._
  import scala.scalajs.js

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  val loginDelay: FiniteDuration = 50.millis

  @JSExport
  def main(): Unit = {
    import scalajs.concurrent.JSExecutionContext.Implicits.queue
    val longRegex = Pattern.compile("\n+\\s*", Pattern.MULTILINE)
    val fetchSearchResults = monadic[Task] {
      val favoriteFilterResponse = Ajax.get(url = "https://auviknetworks.atlassian.net/rest/api/2/filter/favourite", timeout = 4000, headers = Creds.authData).each
      println(s"favoriteFilterResponse ${favoriteFilterResponse.status} ${favoriteFilterResponse.statusText}: ${favoriteFilterResponse.responseText}")
      val favoriteFilters = json.read(favoriteFilterResponse.responseText).arr.map { r =>
        Filter(r.obj("self").str, r.obj("name").str, r.obj("owner").obj("name").str, r.obj("jql").str, r.obj("viewUrl").str)
      }.toVector
      val searchRequests = Task.sequence(favoriteFilters.map(filter => {
        Ajax.post(url = s"https://auviknetworks.atlassian.net/rest/api/2/search/",
          data = json.write(Js.Obj("jql" -> Js.Str(filter.jql), "maxResults" -> Js.Num(10))),
          timeout = 4000, headers = Creds.authData ++ Map("Content-Type" -> "application/json"))
      })).each
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
        println(s"Rendered ${results.length} filters!")
        val container: Element = org.scalajs.dom.document.body.children.namedItem("container")
        if (!js.isUndefined(Addons.Perf)) {
          println("Starting perf")
          Addons.Perf.start()
          println("Rendering DOM")
        }
        ReactDOM.render(searchPage, container)
        if (!js.isUndefined(Addons.Perf)) {
          println("Stopping perf")
          Addons.Perf.stop()
          println("Printing wasted")
          println(Addons.Perf.printWasted(Addons.Perf.getLastMeasurements()))
        }
      }
      override def onError(error: Throwable): Unit = {
        System.err.println("EXCEPTION INTERRUPTED MAIN:")
        error match {
          case AjaxException(req) =>
            System.err.println(s"status code: ${req.status}")
          case _ =>
        }
        error.printStackTrace(System.err)
      }
    }

    {
      import monix.execution.Scheduler.Implicits.global
      fetchSearchResults.runAsync(callback)
    }
  }

}
