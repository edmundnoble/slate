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
    import scala.language.postfixOps

    val panel = style(
      marginLeft(10 px),
      marginRight(10 px),
      marginBottom(10 px),
      //      margin(10 px),
      addClassName("mui-panel")
    )

    val header = style(
      width(100 %%),
      marginTop(-10 px),
      marginBottom(5 px),
      display.inlineBlock
    )

    val title = style(
      addClassName("mui--text-headline"),
      fontFamily(dash.views.Styles.sanFranciscoHeavy),
      display inline
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
    import japgolly.scalajs.react.vdom.all._

    import scalacss.ScalaCssReact._

    ReactComponentB[AppProps]("Expandable content view")
      .initialState[AppState](AppState(AppModel(Nil.right)))
      .renderP { (_, props) =>
        div(
          div(Styles.panel,
            div(Styles.header,
              div(ExpandableContentView.Styles.headerLeft,
                div(AppView.Styles.title,
                  a(props.title.toUpperCase())
                )
              )
            ),
            div(
              ExpandableContentView.Styles.animationGroup(
                props.model.content.fold({
                  ErrorView.builder.build(_)
                }, _.map { k =>
                  ExpandableContentView.builder.build(ExpandableContentProps(k, initiallyExpanded = true))
                })
              )
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
  }


}
