package edmin

import java.util.regex.Pattern

import chrome.app
import chrome.app.runtime.bindings.{LaunchData, Request}
import chrome.app.window.Window
import chrome.app.window.bindings.{BoundsSpecification, CreateWindowOptions}
import org.scalajs.dom.ext.Ajax
import upickle.Js
import upickle.json
import upickle.default._
import pprint._
import utils.ChromeApp
import edmin.SearchPage.{Filter, Issue, SearchResult}
import japgolly.scalajs.react.ReactDOM
import org.scalajs.dom.raw.Element

import scalaz.std.scalaFuture._
import scalaz.syntax.monad._
import scala.concurrent.Future
import scala.scalajs.js.Date
import scala.scalajs.js.annotation.JSExport
import scala.util.Try

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  import scalacss.ScalaCssReact._
  import scalacss.Defaults._

  @JSExport
  def main(): Unit = {
    import scalajs.concurrent.JSExecutionContext.Implicits.queue
    val longRegex = Pattern.compile("\n+\\s*", Pattern.MULTILINE)
    val f = for {
      cookieResp <- Ajax.post("https://auviknetworks.atlassian.net/rest/auth/1/session", timeout = 4000,
        data = json.write(Creds.authData), headers = Map("Content-Type" -> "application/json"))
      session = json.read(cookieResp.responseText).obj("session").obj
      authorizationHeaders = Map("Set-Cookie" -> s"${session("name").str}=${session("value").str}; Path=/; HttpOnly")
      favoriteFilterRequest <- Ajax.get(url = "https://auviknetworks.atlassian.net/rest/api/2/filter/favourite",
        timeout = 4000, headers = authorizationHeaders)
      favoriteFilters = json.read(favoriteFilterRequest.responseText).arr.map { r =>
        Filter(r.obj("self").str, r.obj("name").str, r.obj("owner").obj("name").str, r.obj("jql").str, r.obj("viewUrl").str)
      }
      favoriteFilterJqls = favoriteFilters.map(_.jql)
      searchRequests <- Future.traverse(favoriteFilterJqls)(jql => {
        println("Havin a search")
        Ajax.post(url = s"https://auviknetworks.atlassian.net/rest/api/2/search/",
          data = json.write(Js.Obj("jql" -> Js.Str(jql), "maxResults" -> Js.Num(10))),
          timeout = 4000, headers = authorizationHeaders ++ Map("Content-Type" -> "application/json"))
      })
      searchResults = searchRequests.map(r => json.read(r.responseText).obj("issues").arr.map { issueObj =>
        val fields = issueObj("fields").obj
        val url = issueObj("self").str
        val summary = fields("summary").str
        val key = issueObj("key").str
        val project = fields("project").obj("name").str
        val status = fields("status").obj("name").str
        val shortStatus = new String(status.split(" ").filter(_.nonEmpty).map(s => Character.toUpperCase(s.charAt(0))).toArray)
        val assignee = try {
          Some(fields("assignee").obj("name").str)
        } catch {
          case _: Exception => None
        }
        val reporter = try {
          Some(fields("reporter").obj("name").str)
        } catch {
          case _: Exception => None
        }
        val description = longRegex.matcher(fields("description").str).replaceAll(" ↪ ")
        Issue(
          url, summary, key, project, assignee, reporter, shortStatus, description,
          new Date(fields("created").str).getTime(),
          new Date(fields("updated").str).getTime()
        )
      })
    } yield (favoriteFilters, searchResults)
    f.onSuccess { case r =>
      val render =
        SearchPage.makeSearchPage(r.zipped.map(SearchResult(_, _))(collection.breakOut))
      println(s"Rendered ${r._2.length} filters!")
      Styles.addToDocument()
      val container: Element = org.scalajs.dom.document.body.children.namedItem("container")
      ReactDOM.render(render, container)
//      org.scalajs.dom.document.body.appendChild(render.htmlFrag.render)
    }
    f.onFailure { case e =>
      System.err.println("EXCEPTION INTERRUPTED MAIN:")
      e.printStackTrace(System.err)
    }
  }

}
