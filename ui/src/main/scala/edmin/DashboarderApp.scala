package edmin

import chrome.app
import chrome.app.runtime.bindings.{LaunchData, Request}
import chrome.app.window.Window
import chrome.app.window.bindings.{BoundsSpecification, CreateWindowOptions}
import org.scalajs.dom.ext.Ajax
import upickle.Js
import upickle.json
import upickle.default._
import utils.ChromeApp
import com.thoughtworks.each.Monadic._

import scalaz.std.scalaFuture._
import scalaz.syntax.monad._
import scala.concurrent.Future
import scala.scalajs.js.annotation.JSExport

@JSExport
object DashboarderApp extends scalajs.js.JSApp {
  val defaultDashboardSettings = DashboardSettings(startAt = 0, maxResults = 3)

  case class DashboardSettings(startAt: Int, maxResults: Int)
  object DashboardSettings {
    implicit val pkl = upickle.default.macroRW[DashboardSettings]
  }

  case class Filter(url: String, name: String, owner: String, jql: String)
  case class Issue(url: String, key: String, project: String)

  @JSExport
  def main(): Unit = {
    import scalajs.concurrent.JSExecutionContext.Implicits.queue
    val dashboardSettings = defaultDashboardSettings
    monadic[Future] {
      val cookieResp = Ajax.post("https://auviknetworks.atlassian.net/rest/auth/1/session", timeout = 4000,
        data = json.write(Creds.authData), headers = Map("Content-Type" -> "application/json")).each
      val session = json.read(cookieResp.responseText).obj("session").obj
      val authorizationHeaders = Map("Set-Cookie" -> s"${session("name").str}=${session("value").str}; Path=/; HttpOnly")
      val favoriteFilters = json.read(Ajax.get(url = "https://auviknetworks.atlassian.net/rest/api/2/filter/favourite",
        timeout = 4000, headers = authorizationHeaders).each.responseText).arr.map { r =>
        val filterObj = r.obj
        Filter(filterObj("self").str, filterObj("name").str, filterObj("owner").obj("name").str, filterObj("jql").str)
      }
      val favoriteFilterJqls = favoriteFilters.map(_.jql)
      println(s"Favorite filters: $favoriteFilters")
      val searchRequests = Future.traverse(favoriteFilterJqls)(jql => Ajax.post(url = s"https://auviknetworks.atlassian.net/rest/api/2/search/",
        data = json.write(Js.Obj("jql" -> Js.Str(jql), "maxResults" -> Js.Num(1))),
        timeout = 4000, headers = authorizationHeaders ++ Map("Content-Type" -> "application/json"))
      ).each.map(r => json.read(r.responseText).obj("issues").arr)
      println(s"Search results: $searchRequests")
//      val render = new FilterPage(favoriteFilters, searchRequests) //      println(s"Rendered ${dashboards.length} dashboards!")
      //      org.scalajs.dom.document.body.appendChild(render.htmlFrag.render)
    }
  }

}
