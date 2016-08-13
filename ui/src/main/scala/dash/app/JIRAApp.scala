package dash.app

import java.util.concurrent.TimeUnit

import dash.Util._
import dash._
import dash.models.{ExpandableContentModel, TitledContentModel}
import monix.eval.Task
import monix.scalaz._
import qq.jsc.Json
import upickle.Js

import scala.concurrent.duration._
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
      TitledContentModel(title = s"$status - $key - $summary",
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

  val compiledExtractFavorites =
    StorageProgram.runProgram(DomStorage.Local, getCompiledProgram(qqExtractFavoritesProgram)).flatMap(_.valueOrThrow)

  val compiledExtractIssues =
    StorageProgram.runProgram(DomStorage.Local, getCompiledProgram(qqExtractIssuesProgram)).flatMap(_.valueOrThrow)

  def fetchSearchResults: Task[Seq[ExpandableContentModel]] = {

    implicit val ajaxTimeout = Ajax.Timeout(Duration(4000, TimeUnit.MILLISECONDS))

    for {
      filtersAndResponses <- for {
        favoriteFilterResponse <- Ajax.get(url = "https://jira.atlassian.net/rest/api/2/filter/favourite", headers = Map.empty)//Creds.authData
        extractFavorites <- compiledExtractFavorites
        // TODO: error handling
        favoriteFilters = extractFavorites(Json.read(favoriteFilterResponse.responseText)).map(_.flatMap(Filter.pkl.read.lift(_)))
        filterRequests <- favoriteFilters.flatMap { filters =>
          filters.traverse { filter =>
            Ajax.post(url = "https://jira.atlassian.net/rest/api/2/search/",
              data = Json.write(Js.Obj("jql" -> Js.Str(filter.jql), "maxResults" -> Js.Num(10))),
              headers = //Creds.authData +
                Map("Content-Type" -> "application/json")).strengthL(filter)
          }
        }
      }
        yield filterRequests
      extractIssues <- compiledExtractIssues
      issues <- Task.gatherUnordered(
        filtersAndResponses.map {
          case (filter, response) =>
            def issueToExpandableContentModel(iss: List[Js.Value]) = SearchResult(filter, iss.map(Issue.pkl.read.lift(_))).toExpandableContentModel
            val issuesJson = Json.read(response.responseText)
            extractIssues(issuesJson).map(issueToExpandableContentModel)
        }
      )
    } yield issues

  }

}
