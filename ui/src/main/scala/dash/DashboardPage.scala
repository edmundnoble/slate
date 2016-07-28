package dash

import dash.models.ExpandableContentModel
import dash.views.ReactiveReact.ReactiveState
import dash.views.{ExpandableContentView, Styles}
import japgolly.scalajs.react._
import monix.execution.Scheduler
import monix.reactive.Observable
import qq.Util._

import scalacss.Defaults._

object DashboardPage {

  import japgolly.scalajs.react.vdom.prefix_<^._

  import scala.language.implicitConversions
  import scalacss.ScalaCssReact._

  def makeFilterRow(firstResult: Option[ExpandableContentModel], secondResult: Option[ExpandableContentModel]): ReactElement = {
    def toView(r: ExpandableContentModel) = ExpandableContentView.build(r)
    <.div(Styles.filterContainer,
      <.div(Styles.innerFilterContainer,
        single(firstResult.map(toView))
      ),
      <.div(Styles.innerFilterContainer,
        single(secondResult.map(toView))
      )
    )
  }

  case class SearchPageState(ExpandableContentModels: IndexedSeq[ExpandableContentModel])
    extends ReactiveState[SearchPageState, IndexedSeq[ExpandableContentModel], Unit](ExpandableContentModels, ()) {
    override def setReactive(r: IndexedSeq[ExpandableContentModel]): SearchPageState = copy(ExpandableContentModels = r)
  }

  def makeSearchPage(ExpandableContentModelStream: Observable[IndexedSeq[ExpandableContentModel]])(implicit sch: Scheduler): ReactElement = {
    import views.ReactiveReact._
    reactiveBackendReplace(
      ReactComponentB[Observable[IndexedSeq[ExpandableContentModel]]]("Main search page")
        .initialState(SearchPageState(IndexedSeq.empty))
        .noBackend
        .renderS { ($, state) =>
          <.div(Styles.render[ReactElement],
            ^.id := "react-root",
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
                state.ExpandableContentModels.grouped(2).map(xs => makeFilterRow(xs.headOption, xs.drop(1).headOption)).toSeq
              )
            )
          )
        }
        .domType[TopNode]
    )
      .build(ExpandableContentModelStream)
  }

}

object LinkStyles extends StyleSheet.Standalone {

  import dsl._

  "a" - color(c"#616161")
}
