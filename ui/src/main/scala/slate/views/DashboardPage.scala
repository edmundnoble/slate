package slate
package views

import japgolly.scalajs.react
import japgolly.scalajs.react.CompScope.DuringCallbackU
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import slate.util.ExternalVar
import monix.execution.Scheduler
import slate.app.SlateProgramConfig
import slate.views.AppView.AppProps

import scalacss.Defaults._

object DashboardPage {

  import japgolly.scalajs.react.vdom.all._

  import scalacss.ScalaCssReact._

  def makeAppCell(result: AppProps, extVar: ExternalVar[SlateProgramConfig])(implicit sch: Scheduler): ReactElement = {
    AppView.builder(extVar).build(result)
  }

  case class SearchPageProps(appProps: List[AppProps])

  object SearchPageProps {
    implicit val reusability: Reusability[SearchPageProps] =
      Reusability.caseClass[SearchPageProps]
  }

  type ConfigMap = Map[Int, SlateProgramConfig]

  def makeDashboardPage(extVar: ExternalVar[Map[Int, SlateProgramConfig]], props: SearchPageProps)(implicit sch: Scheduler): ReactComponentU[SearchPageProps, Unit, Unit, react.TopNode] = {
    ReactComponentB[SearchPageProps]("Main dashboard page")
      .stateless
      .renderP(renderDashboard(extVar))
      .domType[TopNode]
      .build(props)
  }

  private def renderDashboard(extVar: ExternalVar[Map[Int, SlateProgramConfig]])
                             ($: DuringCallbackU[SearchPageProps, Unit, Unit],
                              props: SearchPageProps)(implicit sch: Scheduler): ReactElement = {
    div(Styles.layoutContainer,
      div(`class` := "mdl-color--grey-200 mdl-layout mdl-js-layout",
        id := "react-root",
        header(`class` := "mdl-layout__header mdl-shadow--2dp mdl-color--grey-100 is-casting-shadow",
          div(`class` := "mdl-layout__header-row",
            span(Styles.appBarText, "Slate".toUpperCase()),
            div(`class` := "mdl-layout-spacer"),
            div(`class` := "mdl-textfield mdl-js-textfield mdl-textfield--expandable",
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
            props.appProps.map(app =>
              makeAppCell(app, ExternalVar[SlateProgramConfig](() => extVar.getter()(app.id), spc => extVar.setter(extVar.getter() + (app.id -> spc))))
            )
          )
        )
      )
    )
  }

}

object LinkStyles extends StyleSheet.Standalone {

  import dsl._

  "a" - color(Styles.materialGrays)
}
