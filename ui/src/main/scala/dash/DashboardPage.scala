package dash

import dash.Util._
import dash.views.AppView.AppProps
import dash.views.{AppView, Styles}
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

  def makeAppRow(results: List[AppProps])(implicit sch: Scheduler
  ): ReactElement =
    div(Styles.filterContainer,
      results.map(result =>
        div(Styles.innerFilterContainer,
          AppView.builder.build(result)
        )
      )
    )

  case class AppBarState(scrollY: Double)

  object AppBarState {
    implicit val reusability: Reusability[AppBarState] =
      Reusability.byRefOr_==[AppBarState]
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

  case class SearchPageState(appProps: List[AppProps])

  object SearchPageState {
    implicit val reusability: Reusability[SearchPageState] =
      Reusability.caseClass[SearchPageState]
  }

  def makeDashboardPage(appbarProps: Observable[AppBarState])(implicit sch: Scheduler
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
                state.appProps
                  .grouped(2)
                  .map(makeAppRow).toSeq: _*
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
