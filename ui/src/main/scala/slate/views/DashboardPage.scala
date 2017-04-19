package slate
package views

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala
import japgolly.scalajs.react.component.ScalaBuilder.Lifecycle.RenderScope
import japgolly.scalajs.react.extra.Reusability
import slate.util.ExternalVar
import monix.execution.Scheduler
import slate.app.SlateApp.AppState
import slate.app.SlateProgramConfig
import slate.views.AppView.AppProps

import scalacss.Defaults._

object DashboardPage {

  import scalacss.ScalaCssReact._

  def makeAppCell(result: AppProps, extVar: ModelProxy[SlateProgramConfig])(implicit sch: Scheduler): VdomElement = {
    VdomElement(AppView.builder(extVar).build.apply(result).raw)
  }

  case class SearchPageProps(appProps: List[AppProps])

  object SearchPageProps {
    implicit val reusability: Reusability[SearchPageProps] =
      Reusability.caseClass[SearchPageProps]
  }

  type ConfigMap = Map[Int, SlateProgramConfig]

  def makeDashboardPage(implicit sch: Scheduler): Scala.Unmounted[SearchPageProps, Unit, Unit] = {
    ScalaComponent.builder[ModelProxy[AppState]]("Main dashboard page")
      .stateless
      .noBackend
      .renderP(renderDashboard)
      .build.apply(props)
  }

  private def renderDashboard($: RenderScope[ModelProxy[AppState], Unit, Unit],
                              props: ModelProxy[AppState])(implicit sch: Scheduler): VdomElement = {
    import japgolly.scalajs.react.vdom.all._

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
        HtmlTag("main")(Styles.layoutContent,
          div((Styles.container: TagMod) ::
            props.zoom(_.appProps).apply().map(app =>
              makeAppCell(app, ExternalVar[SlateProgramConfig](() => extVar.getter()(app.id), spc => extVar.setter(extVar.getter() + (app.id -> spc))))
            ): _*
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
