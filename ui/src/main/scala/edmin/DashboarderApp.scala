package edmin

import java.util.regex.Pattern

import chrome.app
import chrome.app.runtime.bindings.{LaunchData, Request}
import chrome.app.window.Window
import chrome.app.window.bindings.{BoundsSpecification, CreateWindowOptions}
import upickle.Js
import upickle.json
import upickle.default._
import pprint._
import utils.ChromeApp
import edmin.SearchPage.{Filter, Issue, SearchResult}
import japgolly.scalajs.react.{Addons, ReactDOM}
import monix.eval.{Callback, Task}
import monix.eval.Task.instances
import org.scalajs.dom.XMLHttpRequest
import org.scalajs.dom.raw.Element

import scalaz.std.scalaFuture._
import scalaz.syntax.monad._
import scalaz.std.vector._
import scala.concurrent.Future
import scala.scalajs.js.Date
import scala.scalajs.js.annotation.JSExport
import scala.util.{Failure, Success, Try}

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  import scalacss.ScalaCssReact._
  import scalacss.Defaults._

  @JSExport
  def main(): Unit = {
    import scalajs.concurrent.JSExecutionContext.Implicits.queue
    val longRegex = Pattern.compile("\n+\\s*", Pattern.MULTILINE)
    val fetchSearchResults = for {
      cookieResp <- Ajax.post("https://auviknetworks.atlassian.net/rest/auth/1/session", timeout = 4000,
        data = json.write(Creds.authData), headers = Map("Content-Type" -> "application/json"))
      session = json.read(cookieResp.responseText).obj("session").obj
      authorizationHeaders = Map("Set-Cookie" -> s"${session("name").str}=${session("value").str}; Path=/; HttpOnly")
      favoriteFilterRequest <- Ajax.get(url = "https://auviknetworks.atlassian.net/rest/api/2/filter/favourite",
        timeout = 4000, headers = authorizationHeaders)
      favoriteFilters = json.read(favoriteFilterRequest.responseText).arr.map { r =>
        Filter(r.obj("self").str, r.obj("name").str, r.obj("owner").obj("name").str, r.obj("jql").str, r.obj("viewUrl").str)
      }.toVector
      favoriteFilterJqls = favoriteFilters.map(_.jql)
      searchRequests <- Task.sequence(favoriteFilterJqls.map(jql => {
        Ajax.post(url = s"https://auviknetworks.atlassian.net/rest/api/2/search/",
          data = json.write(Js.Obj("jql" -> Js.Str(jql), "maxResults" -> Js.Num(10))),
          timeout = 4000, headers = authorizationHeaders ++ Map("Content-Type" -> "application/json"))
      }))
      searchResults = searchRequests.map(r => json.read(r.responseText).obj("issues").arr.map { issueObj =>
        val fields = issueObj("fields").obj
        val url = issueObj("self").str
        val summary = fields("summary").str
        val key = issueObj("key").str
        val project = fields("project").obj("name").str
        val status = fields("status").obj("name").str
        val shortStatus = new String(status.split(" ").filter(_.nonEmpty).map(s => Character.toUpperCase(s.charAt(0))).toArray)
        val assignee = CanThrow(fields("assignee").obj("name").str).toOption
        val reporter = CanThrow(fields("reporter").obj("name").str).toOption
        val description = longRegex.matcher(fields("description").str).replaceAll(" ↪ ")
        Issue(
          url, summary, key, project, assignee, reporter, shortStatus, description,
          new Date(fields("created").str).getTime(),
          new Date(fields("updated").str).getTime()
        )
      })
    } yield (favoriteFilters, searchResults).zipped.map(SearchResult(_, _))(collection.breakOut)

    val callback = new Callback[IndexedSeq[SearchResult]] {
      override def onSuccess(results: IndexedSeq[SearchResult]): Unit = {
        val searchPage =
          SearchPage.makeSearchPage(results)
        println(s"Rendered ${results.length} filters!")
        val container: Element = org.scalajs.dom.document.body.children.namedItem("container")
        if (!scalajs.js.isUndefined(Addons.Perf)) {
          println("Starting perf")
          Addons.Perf.start()
          println("Rendering DOM")
        }
        ReactDOM.render(searchPage, container)
        if (!scalajs.js.isUndefined(Addons.Perf)) {
          println("Stopping perf")
          Addons.Perf.stop()
          println("Printing wasted")
          println(Addons.Perf.printWasted(Addons.Perf.getLastMeasurements()))
        }
      }
      override def onError(error: Throwable): Unit = {
        System.err.println("EXCEPTION INTERRUPTED MAIN:")
        error.printStackTrace(System.err)
      }
    }

    {
      import monix.execution.Scheduler.Implicits.global
      fetchSearchResults.runAsync(callback)
    }
  }

}
