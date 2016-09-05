package dash
package app

import java.util.concurrent.TimeUnit

import dash.Util._
import dash._
import qq.ajax.Ajax
import dash.models.{ExpandableContentModel, TitledContentModel}
import monix.eval.Task
import monix.scalaz._
import qq.QQCompiler.{CompiledFilter, OrCompilationError}
import qq.Runner
import qq.jsc.{JSRuntime, Json}
import upickle.Js
import upickle.Js.Value

import scala.concurrent.duration._
import scala.scalajs.js
import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.traverse._

object JIRAApp extends DashApp {

  val qqExtractContentProgram =
    raw"""
def authHeaders: { Authorization: "Basic ${Creds.hashedJiraCreds}" };

def extractIssues: .issues.[] | {
  url: .self,
  summary: .fields.summary,
  key,
  project: .fields.project.name,
  description: (.fields.description | replaceAll("\n+\\s*"; " ↪ ")),
  status: .fields.status.name
};

def issues: httpPost("https://dashboarder.atlassian.net/rest/api/2/search/"; {};
                     { jql: .jql, maxResults: 10 }; authHeaders + { ("Content-Type"): "application/json" }) | extractIssues;

def extractFilters: .[] | {
  url: .self,
  name,
  owner: .owner.name,
  jql,
  issues: [issues],
  viewUrl
};

def filters: httpGet("https://dashboarder.atlassian.net/rest/api/2/filter/favourite"; {};
                     {}; authHeaders) | extractFilters;

def contentFromIssue: { title: .status + " - " + .key + " - " + .summary,
                        titleUrl: "https://dashboarder.atlassian.net/browse/" + .key,
                        content: .description };

def contentFromFilter: { title: .name,
                         titleUrl: .viewUrl,
                         content: [.issues | .[] | contentFromIssue] };

filters | contentFromFilter
"""

  def fetchSearchResults: Task[Seq[ExpandableContentModel]] = {

    implicit val ajaxTimeout = Ajax.Timeout(Duration(4000, TimeUnit.MILLISECONDS))

    import qq.Platform.Rec._

    for {
      extractContent <- StorageProgram.runProgram(DomStorage.Local, getCachedCompiledProgram(qqExtractContentProgram)).flatMap(_.valueOrThrow)
      content <- extractContent(null.asInstanceOf[Any])
      readContent = content.flatMap(i => ExpandableContentModel.pkl.read.lift(Json.jsToUpickleRec(i)))
    } yield readContent
  }

}
