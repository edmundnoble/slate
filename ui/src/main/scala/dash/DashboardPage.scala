package dash

import dash.models.{ExpandableContentModel, TitledContentModel}
import dash.views.{ExpandableContentView, Styles}
import japgolly.scalajs.react._
import qq.Util._

import scalacss.Defaults._

object DashboardPage {

  import japgolly.scalajs.react.vdom.prefix_<^._

  import scala.language.implicitConversions
  import scalacss.ScalaCssReact._

  case class Filter(url: String, name: String, owner: String, jql: String, viewUrl: String)
  object Filter {
    implicit val pkl = upickle.default.macroRW[Filter]
  }

  case class Issue(url: String, summary: String, key: String, project: String, assignee: Option[String], reporter: Option[String],
                   status: String, description: String, created: Double, updated: Double) {
    def toTitledContentModel: TitledContentModel =
      TitledContentModel(title = s"$status - $key - $summary",
        titleUrl = Some("https://auviknetworks.atlassian.net/browse/" + key),
        content = description)
  }
  object Issue {
    implicit val pkl = upickle.default.macroRW[Issue]
  }

  case class SearchResult(filter: Filter, issues: Seq[Issue]) {
    def toExpandableContentModel: ExpandableContentModel =
      ExpandableContentModel(title = filter.name, titleUrl = Some(filter.viewUrl), content = issues.map(_.toTitledContentModel))
  }
  object SearchResult {
    implicit val pkl = upickle.default.macroRW[SearchResult]
  }

  def makeFilterRow(firstResult: Option[SearchResult], secondResult: Option[SearchResult]): ReactElement = {
    def toView(r: SearchResult) = ExpandableContentView.Html(r.toExpandableContentModel)
    <.div(Styles.filterContainer,
      <.div(Styles.innerFilterContainer,
        single(firstResult.map(toView))
      ),
      <.div(Styles.innerFilterContainer,
        single(secondResult.map(toView))
      )
    )
  }

  def makeSearchPage(searchResults: IndexedSeq[SearchResult]): ReactElement = {
    val htmlFrag =
      <.div(Styles.render[ReactElement],
        <.div(Styles.appBar,
          <.table(
            <.tr(Styles.appBarRow,
              <.td(Styles.appBarText,
                "Dashboarder")
            )
          )
        ),
        <.div(
          <.div(Styles.container,
            searchResults.grouped(2).map(xs => makeFilterRow(xs.headOption, xs.drop(1).headOption)).toSeq
          )
        )
      )

    ReactComponentB[Unit]("JIRA search page")
      .render(_ => htmlFrag)
      .build()
  }

}

object LinkStyles extends StyleSheet.Standalone {

  import dsl._

  "a" - color(c"#616161")
}
