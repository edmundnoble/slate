package edmin

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

import scalaz.std.scalaFuture._
import scalaz.syntax.monad._
import scala.concurrent.Future
import scala.scalajs.js.Date
import scala.scalajs.js.annotation.JSExport
import scala.util.Try

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  @JSExport
  def main(): Unit = {
    import scalajs.concurrent.JSExecutionContext.Implicits.queue
    for {
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
        println(s"Url: $url")
        val key = issueObj("key").str
        println(s"Key: $key")
        val project = fields("project").obj("name").str
        println(s"Project: $project")
        val status = fields("status").obj("name").str
        println(s"Status: $status")
        val assignee = try { Some(fields("assignee").obj("name").str) } catch { case _: Exception => None }
        println(s"Assignee: $assignee")
        val reporter = try { Some(fields("reporter").obj("name").str) } catch { case _: Exception => None }
        println(s"Reporter: $reporter")
        Issue(
          url, key, project, assignee, reporter, status
          //          created = new Date(fields("created").str),
          //          updated = new Date(fields("updated").str)
        )
      })
    } yield {
      println("Done getting issues!")
      val render =
        new SearchPage((favoriteFilters, searchResults).zipped.map(SearchResult(_, _))(collection.breakOut))
      println(s"Rendered ${favoriteFilters.length} filters!")
      org.scalajs.dom.document.body.appendChild(render.htmlFrag.render)
    }
  }

}
