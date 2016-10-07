package dash
package views

import dash.models.ExpandableContentModel
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import monix.execution.Scheduler

import scala.language.higherKinds
import scalacss.Defaults._
import scalaz.std.list._
import scalaz.syntax.std.boolean._
import Util._

object ExpandableContentView {
  object Styles extends StyleSheet.Inline {

    import dsl._
    import dash.views.Styles._

    import scala.language.postfixOps

    val filterButtonIcon = style(
      addClassName("material-icons"),
      (transition := "transform 0.3s ease-out").important,
      marginTop(5 px)
    )

    val filterButtonExpanded = style(
      transform := "rotateX(-180deg)",
      perspective(600 pt)
    )

    val base = style(
      width(100 %%),
      addClassNames("mdl-color--grey-100", "mdl-color-text--grey-600"),
      overflow.hidden,
      marginRight(20 px),
      marginBottom(20 px),
      marginTop(5 px)
    )

    val header = style(
      width(100 %%),
      addClassName("mdl-card__title-text"),
      borderBottom.rgba(0, 0, 0, .13),
      borderBottomStyle.solid,
      borderBottomWidth(1 px),
      display.inlineBlock,
      marginTop(-10 px)
    )

    val expandToggleButton = style(
      addClassNames("mdl-button", "mdl-js-button", "mdl-js-ripple-effect"),
      minWidth(56 px).important,
      marginRight(10 px),
      marginTop(5 px),
      float.right,
      lineHeight.`0`.important
    )

    val headerLeft = style(
      marginLeft(10 px),
      float left,
      marginTop(10 px),
      marginBottom(10 px)
    )

    val number = style(
      fontSize(22 px),
      fontWeight._600,
      addClassName("mdl-color-text--grey-800"),
      fontFamily(akrobat),
      marginLeft(5 px),
      paddingRight(5 px),
      display inline
    )

    val title = style(
      fontSize(22 px),
      fontWeight._300,
      addClassName("mdl-color-text--grey-500"),
      fontFamily(akrobat),
      display inline
    )

    val content = style(
      addClassName("mdl-list"),
      paddingTop(5 px).important,
      marginLeft(10 px),
      paddingRight(10 px)
    )

    val animationGroup = new dash.views.ScrollFadeContainer("expandableContentView")

  }

  final case class ExpandableState(expanded: Boolean) {
    final def toggleExpanded = copy(expanded = !expanded)
  }

  object ExpandableState {
    implicit val reusability: Reusability[ExpandableState] =
      Reusability.caseClass[ExpandableState]
  }

  final case class ExpandableContentProps(model: ExpandableContentModel, initiallyExpanded: Boolean)

  object ExpandableContentProps {
    implicit val reusability: Reusability[ExpandableContentProps] =
      Reusability.caseClass[ExpandableContentProps]
  }

  def builder(implicit sch: Scheduler
             ): ReactComponentB[ExpandableContentProps, ExpandableState, Unit, TopNode] = {
    import japgolly.scalajs.react.vdom.all._

    import scalacss.ScalaCssReact._

    def buttonStyleForState(state: ExpandableState): TagMod = {
      (state.expanded ?? (Styles.filterButtonExpanded: TagMod)) +
        (Styles.filterButtonIcon: TagMod) + ("expand_more": TagMod)
    }

    ReactComponentB[ExpandableContentProps]("Expandable content view")
      .initialState_P[ExpandableState](props => ExpandableState(expanded = props.initiallyExpanded))
      .renderPS { ($, props, state) =>
        val titleLink = href := props.model.titleUrl
        div(key := props.model.title, Styles.base,
          div(Styles.header,
            div(Styles.headerLeft,
              span(Styles.number,
                props.model.content.length.toString()),
              a(Styles.title, props.model.title, titleLink)
            ),
            button(Styles.expandToggleButton,
              onClick --> $.modState(_.toggleExpanded),
              i(buttonStyleForState(state))
            )
          ),
          span(Styles.content,
            Styles.animationGroup(
              state.expanded ?? props.model.content.map(TitledContentView.builder.build(_)): _*
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
  }

}
