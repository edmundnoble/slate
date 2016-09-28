package dash
package views

import dash.models.AppModel
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.{ReactComponentB, TopNode}
import monix.execution.Scheduler
import monix.reactive.Observable
import dash.Util.observableReusability
import dash.views.ExpandableContentView.ExpandableContentProps

import scalacss.Defaults._
import scalaz.\/
import scalaz.syntax.either._

object AppView {

  object Styles extends StyleSheet.Inline {

    import dsl._

    val panel = style(
      addClassName("mui-panel")
    )

  }

  final case class AppState(model: AppModel)

  object AppState {
    implicit val reusability: Reusability[AppState] =
      Reusability.caseClass[AppState]
  }

  final case class AppProps(title: String, model: AppModel)

  object AppProps {
    implicit val reusability: Reusability[AppProps] =
      Reusability.byRefOr_==
  }

  def builder(implicit sch: Scheduler
             ): ReactComponentB[AppProps, AppState, Unit, TopNode] = {
    import dash.views.ReactiveReact._
    import japgolly.scalajs.react.vdom.all._

    import scalacss.ScalaCssReact._

    ReactComponentB[AppProps]("Expandable content view")
      .initialState[AppState](AppState(AppModel(Nil.right)))
      .renderP { (_, props) =>
        div(
          div(Styles.panel,
            div(ExpandableContentView.Styles.header,
              div(ExpandableContentView.Styles.headerLeft,
                div(ExpandableContentView.Styles.title,
                  a(props.title)
                )
              )
            ),
            div(
              ExpandableContentView.Styles.animationGroup(
                props.model.content.fold({
                  ErrorView.builder.build(_)
                }, _.map { k =>
                  ExpandableContentView.builder.build(ExpandableContentProps(k, initiallyExpanded = false))
                })
              )
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
  }


}
