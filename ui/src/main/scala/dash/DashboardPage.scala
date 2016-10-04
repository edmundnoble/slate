package dash

import dash.Util._
import dash.views.AppView.AppProps
import dash.views.{AppView, Styles}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import monix.execution.Scheduler
import monix.reactive.Observable

import scalacss.Defaults._
import scalaz.\/

object DashboardPage {

  import japgolly.scalajs.react.vdom.all._
  import views.ReactiveReact._

  import scala.language.implicitConversions
  import scalacss.ScalaCssReact._

  def makeAppRow(results: List[AppProps])(implicit sch: Scheduler
  ): ReactElement =
    div(Styles.filterContainer,
      Styles.dashboardContainer(
        results.map(result =>
          div(key := result.id,
            Styles.innerFilterContainer,
            AppView.builder.build(result)
          )
        ): _*
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
                "Dashboarder".toUpperCase()
              )
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .reactiveReplace

  case class SearchPageProps(appProps: List[AppProps])

  object SearchPageProps {
    implicit val reusability: Reusability[SearchPageProps] =
      Reusability.caseClass[SearchPageProps]
  }

  def makeDashboardPage(appbarProps: Observable[AppBarState])(implicit sch: Scheduler
  ): ReactComponentB[SearchPageProps, Unit, Unit, TopNode] =
    ReactComponentB[SearchPageProps]("Main search page")
      .stateless
      .render_P { (props) =>
        div(
          id := "react-root",
          appBar.build(appbarProps),
          div(
            div(Styles.container,
              props.appProps
                .grouped(2)
                .map(makeAppRow).toSeq
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)

}

object LinkStyles extends StyleSheet.Standalone {

  import dsl._

  "a" - color(Styles.materialGrays)
}
