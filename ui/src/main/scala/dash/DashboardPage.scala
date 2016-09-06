package dash

import dash.Util._
import dash.models.ExpandableContentModel
import dash.views.ExpandableContentView.ExpandableContentProps
import dash.views.{ExpandableContentView, Styles}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import monix.execution.Scheduler
import monix.reactive.Observable

import scalacss.Defaults._

object DashboardPage {

  import japgolly.scalajs.react.vdom.all._
  import views.ReactiveReact._

  import scala.language.implicitConversions
  import scalacss.ScalaCssReact._

  def makeFilterRow(results: Seq[Observable[ExpandableContentModel]])(implicit sch: Scheduler
  ): ReactElement =
    div(Styles.filterContainer,
      results.map(result =>
        div(Styles.innerFilterContainer,
          ExpandableContentView.builder.build(ExpandableContentProps(result, expanded = false))
        )
      )
    )

  case class AppBarState(scrollY: Double)

  object AppBarState {
    implicit val reusability = Reusability.byRefOr_==[AppBarState]
  }

  def appBar(implicit sch: Scheduler
            ): ReactComponentB[Observable[AppBarState], AppBarState, Unit, TopNode] =
    ReactComponentB[Observable[AppBarState]]("App bar")
      .initialState(AppBarState(0.0))
      .render_S { _ =>
        div(Styles.appBar,
          table(
            tr(Styles.appBarRow,
              td(Styles.appBarText,
                "Dashboarder"
              )
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .reactiveReplace

  case class SearchPageState(expandableContentModels: List[ExpandableContentModel])

  object SearchPageState {
    implicit val reusability = Reusability.caseClass[SearchPageState]
  }

  def makeSearchPage(appbarProps: Observable[AppBarState])(implicit sch: Scheduler
  ): ReactComponentB[Observable[SearchPageState], SearchPageState, Unit, TopNode] =
    ReactComponentB[Observable[SearchPageState]]("Main search page")
    .initialState(SearchPageState(Nil))
    .renderS { (_, state) =>
      div(
        id := "react-root",
        appBar.build(appbarProps),
        div(
          div(Styles.container,
            Styles.dashboardContainer(
              state.expandableContentModels
                .iterator
                .grouped(2)
                .map(exs => makeFilterRow(exs.map(Observable.now))).toSeq: _*
            )
          )
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .reactiveReplace

}

object LinkStyles extends StyleSheet.Standalone {

  import dsl._

  "a" - color(c"#616161")
}
