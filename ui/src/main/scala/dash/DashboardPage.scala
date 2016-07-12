package dash

import dash.models.{ExpandableContentModel, TitledContentModel}
import dash.views.ReactiveReact.ReactiveState
import dash.views.{ExpandableContentView, Styles}
import japgolly.scalajs.react._
import monix.execution.Scheduler
import monix.reactive.Observable
import qq.Util._

import scala.collection.mutable
import scala.scalajs.js
import scalaz.std.list._
import scalacss.Defaults._

object DashboardPage {

  import japgolly.scalajs.react.vdom.prefix_<^._

  import scala.language.implicitConversions
  import scalacss.ScalaCssReact._

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

  case class SearchResult(filter: Filter, issues: Seq[Issue]) {
    def toExpandableContentModel: ExpandableContentModel =
      ExpandableContentModel(title = filter.name, titleUrl = Some(filter.viewUrl), content = issues.map(_.toTitledContentModel))
  }
  object SearchResult {
    implicit val pkl = upickle.default.macroRW[SearchResult]
  }

  def makeFilterRow(firstResult: Option[SearchResult], secondResult: Option[SearchResult]): ReactElement = {
    def toView(r: SearchResult) = ExpandableContentView.build(r.toExpandableContentModel)
    <.div(Styles.filterContainer,
      <.div(Styles.innerFilterContainer,
        single(firstResult.map(toView))
      ),
      <.div(Styles.innerFilterContainer,
        single(secondResult.map(toView))
      )
    )
  }

  case class SearchPageState(searchResults: IndexedSeq[SearchResult])
    extends ReactiveState[SearchPageState, IndexedSeq[SearchResult], Unit](searchResults, ()) {
    override def setReactive(r: IndexedSeq[SearchResult]): SearchPageState = copy(searchResults = r)
  }

  def makeSearchPage(searchResultStream: Observable[IndexedSeq[SearchResult]])(implicit sch: Scheduler): ReactElement = {
    import views.ReactiveReact._
    reactiveBackendReplace(
      ReactComponentB[Observable[IndexedSeq[SearchResult]]]("Main search page")
        .initialState(SearchPageState(IndexedSeq.empty))
        .noBackend
        .renderS { ($, state) =>
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
                state.searchResults.grouped(2).map(xs => makeFilterRow(xs.headOption, xs.drop(1).headOption)).toSeq
              )
            )
          )
        }
        .domType[TopNode]
    )
      .build(searchResultStream)
  }

}

object LinkStyles extends StyleSheet.Standalone {

  import dsl._

  "a" - color(c"#616161")
}
