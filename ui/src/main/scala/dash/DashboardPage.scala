package dash

import dash.models.ExpandableContentModel
import dash.views.ReactiveReact.ReactiveState
import dash.views.{ExpandableContentView, Styles}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import monix.execution.Scheduler
import monix.reactive.Observable
import qq.Util._
import dash.Util._
import japgolly.scalajs.react.vdom.ReactNodeFrag

import scalacss.Defaults._

object DashboardPage {

  import japgolly.scalajs.react.vdom.all._

  import scala.language.implicitConversions
  import scalacss.ScalaCssReact._

  def makeFilterRow(results: Seq[ExpandableContentModel]): ReactElement = {
    div(Styles.filterContainer,
      results.map(result =>
        div(Styles.innerFilterContainer,
          ExpandableContentView.builder.build(result)
        )
      )
    )
  }

  case class SearchPageState(expandableContentModels: Seq[ExpandableContentModel])
    extends ReactiveState[SearchPageState, Seq[ExpandableContentModel], Unit](expandableContentModels, ()) {
    override def setReactive(r: Seq[ExpandableContentModel]): SearchPageState = {
      copy(expandableContentModels = r)
    }
  }

  object SearchPageState {
    implicit val reusability = Reusability.byRefOr_==[SearchPageState]
  }

  def makeSearchPage(implicit sch: Scheduler
                    ): ReactComponentB[Observable[Seq[ExpandableContentModel]], SearchPageState, Unit, TopNode] = {
    import views.ReactiveReact._
    ReactComponentB[Observable[Seq[ExpandableContentModel]]]("Main search page")
      .initialState(SearchPageState(IndexedSeq.empty))
      .renderS { (_, state) =>
        div(
          id := "react-root",
          div(Styles.appBar,
            table(
              tr(Styles.appBarRow,
                td(Styles.appBarText,
                  "Dashboarder"
                )
              )
            )
          ),
          div(
            div(Styles.container,
              Styles.dashboardContainer(
                state.expandableContentModels.grouped(2).map(makeFilterRow).toSeq: _*
              )
            )
          )
        ).render
      }
      .configure(Reusability.shouldComponentUpdate)
      .reactiveReplace
  }

}

object LinkStyles extends StyleSheet.Standalone {

  import dsl._

  "a" - color(c"#616161")
}
