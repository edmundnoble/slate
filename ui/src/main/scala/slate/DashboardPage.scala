package slate

import slate.views.AppView.AppProps
import slate.views.{AppView, Styles}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import monix.execution.Scheduler
import monix.reactive.Observable

import scalacss.Defaults._
import cats.data.Xor

object DashboardPage {

  import japgolly.scalajs.react.vdom.all._

  import scala.language.implicitConversions
  import scalacss.ScalaCssReact._

  def makeAppCell(result: AppProps)(implicit sch: Scheduler): ReactElement =
    AppView.builder.build(result)

  case class SearchPageProps(appProps: List[AppProps])

  object SearchPageProps {
    implicit val reusability: Reusability[SearchPageProps] =
      Reusability.caseClass[SearchPageProps]
  }

  def makeDashboardPage(implicit sch: Scheduler): ReactComponentB[SearchPageProps, Unit, Unit, TopNode] =
    ReactComponentB[SearchPageProps]("Main dashboard page")
      .stateless
      .render_P(renderDashboard)
      .configure(Reusability.shouldComponentUpdate)

  private def renderDashboard(props: SearchPageProps)(implicit sch: Scheduler): ReactElement = {
    div(Styles.layoutContainer,
      div(`class` := "mdl-layout mdl-js-layout", "data-upgraded".reactAttr := ",MaterialLayout",
        id := "react-root",
        header(`class` := "mdl-layout__header mdl-shadow--2dp mdl-color--grey-100 is-casting-shadow",
          div(`class` := "mdl-layout__header-row",
            span(Styles.appBarText, "Slate".toUpperCase()),
            div(`class` := "mdl-layout-spacer"),
            div(`class` := "mdl-textfield mdl-js-textfield mdl-textfield--expandable is-upgraded",
              label(`class` := "mdl-button mdl-js-button mdl-button--icon", `for` := "search"),
              i(`class` := "material-icons", "search")
            ),
            div(`class` := "mdl-textfield__expandable-holder",
              input(`class` := "mdl-textfield__input", `type` := "text", id := "search"),
              label(`class` := "mdl-textfield__label", `for` := "search", "Enter your query...")
            )
          )
        ),
        "main".reactTag(Styles.layoutContent,
          div(Styles.container,
            props.appProps.map(makeAppCell))
        )
      )
    )
  }
}

object LinkStyles extends StyleSheet.Standalone {

  import dsl._

  "a" - color(Styles.materialGrays)
}
