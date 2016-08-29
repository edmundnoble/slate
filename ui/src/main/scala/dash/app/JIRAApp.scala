package dash.app

import java.util.concurrent.TimeUnit

import dash.Util._
import dash._
import qq.ajax.Ajax
import dash.models.{ExpandableContentModel, TitledContentModel}
import monix.eval.Task
import monix.scalaz._
import qq.jsc.Json
import upickle.Js
import upickle.Js.Value

import scala.concurrent.duration._
import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.traverse._

object JIRAApp extends DashApp {

  case class Filter(url: String, name: String, owner: String, jql: String, viewUrl: String)
  object Filter {
    implicit val pkl = SnakeOptionPickle.macroRW[Filter]
  }

  case class Issue(url: String, summary: String, key: String, project: String,
                   status: String, description: String) {
    def toTitledContentModel: TitledContentModel =
      TitledContentModel(title = status + " - " + key + " - " + summary,
        titleUrl = Some("https://auviknetworks.atlassian.net/browse/" + key),
        content = description)
  }
  object Issue {
    implicit val pkl = SnakeOptionPickle.macroRW[Issue]
  }

  case class SearchResult(filter: Filter, issues: Seq[Option[Issue]]) {
    // TODO: error handling
    def toExpandableContentModel: ExpandableContentModel =
      ExpandableContentModel(title = filter.name, titleUrl = Some(filter.viewUrl),
        content = issues.flatMap(_.map(_.toTitledContentModel))(collection.breakOut))
  }
  object SearchResult {
    implicit val pkl = upickle.default.macroRW[SearchResult]
  }

  val qqExtractFavoritesProgram =
    """
.[] | {
  url: .self,
  name,
  owner: .owner.name,
  jql,
  viewUrl
}
"""

  val qqExtractIssuesProgram =
    """
.[].issues.[] | {
  url: .self,
  summary: .fields.summary,
  key,
  project: .fields.project.name,
  description: (.fields.description | replaceAll("\n+\\s*"; " ↪ ")),
  status: .fields.status.name
}"""

  def fetchSearchResults: Task[Seq[ExpandableContentModel]] = {

    implicit val ajaxTimeout = Ajax.Timeout(Duration(4000, TimeUnit.MILLISECONDS))

    import qq.Platform.Rec._

    for {
      filtersAndResponses <- for {
        favoriteFilterResponse <- Ajax.get(url = "https://jira.atlassian.net/rest/api/2/filter/favourite", headers = Map.empty) //Creds.authData
        // TODO: error handling
        upickle = Json.stringToUpickle(favoriteFilterResponse.responseText).valueOr(???)
        extractFavorites <- StorageProgram.runProgram(DomStorage.Local, getCompiledProgram(qqExtractFavoritesProgram)).flatMap(_.valueOrThrow)
        favoriteFilters = extractFavorites(upickle).map(_.flatMap(i => Filter.pkl.read.lift(Json.jsToUpickleRec(i))))
        filterRequests <- favoriteFilters.flatMap { filters =>
          filters.traverse { filter =>
            Ajax.post(url = "https://jira.atlassian.net/rest/api/2/search/",
              data = Json.upickleToString(Js.Obj("jql" -> Js.Str(filter.jql), "maxResults" -> Js.Num(10))),
              headers = //Creds.authData +
                Map("Content-Type" -> "application/json")).strengthL(filter)
          }
        }
      } yield filterRequests
      extractIssues <- StorageProgram.runProgram(DomStorage.Local, getCompiledProgram(qqExtractIssuesProgram)).flatMap(_.valueOrThrow)
      issues <- Task.gather(
        filtersAndResponses.map {
          case (filter, response) =>
            def issueToExpandableContentModel(iss: List[Any]) = SearchResult(filter, iss.map(i => Issue.pkl.read.lift(Json.jsToUpickleRec(i)))).toExpandableContentModel
            val issuesJson = Json.stringToJs(response.responseText).valueOr(???)
            extractIssues(issuesJson).map(issueToExpandableContentModel)
        }
      )
    } yield issues

  }

}
