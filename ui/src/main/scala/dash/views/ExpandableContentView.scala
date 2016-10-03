package dash
package views

import dash.models.{ExpandableContentModel, TitledContentModel}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import monix.execution.Scheduler

import scala.language.higherKinds
import scalacss.Defaults._
import scalaz.Memo
import scalaz.std.list._
import scalaz.syntax.std.boolean._

object ExpandableContentView {
  object Styles extends StyleSheet.Inline {

    import dsl._
    import dash.views.Styles._

    import scala.language.postfixOps

    val filterButtonIcon = style(
      addClassName("material-icons"),
      (transition := "transform 0.3s ease-out").important
    )

    val filterButtonExpanded = style(
      transform := "rotateX(-180deg)",
      perspective(600 pt)
    )

    val innerFilterContainer = style(
      addClassName("mui-col-md-6")
    )

    val filterContainer = style(
      addClassName("mui-row")
    )

    val base = style(
      overflow.hidden,
      marginLeft(10 px),
      marginRight(10 px),
      marginBottom(10 px)
    )

    val header = style(
      addClassName("mui--divider-bottom"),
      width(100 %%),
      display.inlineBlock,
      marginTop(-10 px)
    )

    val filterButton = style(
      addClassNames("mui-btn", "mui-btn--raised"),
      float.right,
      lineHeight.`0`.important
    )

    val headerLeft = style(
      float left,
      marginTop(10 px),
      marginBottom(10 px)
    )

    val number = style(
      addClassName("mui--text-title"),
      fontFamily(sanFranciscoSubheavy),
      paddingRight(10 px),
      display inline
    )

    val title = style(
      addClassName("mui--text-title"),
      fontFamily(sanFranciscoSubheavy),
      display inline
    )

    val animationGroup = new dash.views.Styles.ScrollFadeContainer("filter-group")

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

    def buttonStyleForState(state: ExpandableState): Seq[TagMod] = {
      val otherStyles = (Styles.filterButtonIcon: TagMod) :: ("expand_more": TagMod) :: Nil
      if (state.expanded) (Styles.filterButtonExpanded: TagMod) :: otherStyles
      else otherStyles
    }
    val makeContent =
      Memo.mutableHashMapMemo[List[TitledContentModel], List[ReactComponentU[_, _, _, _]]]((_: List[TitledContentModel]).map(TitledContentView.builder.build(_)))

    ReactComponentB[ExpandableContentProps]("Expandable content view")
      .initialState_P[ExpandableState](props => ExpandableState(expanded = props.initiallyExpanded))
      .renderPS { ($, props, state) =>
        val titleLink = href := props.model.titleUrl
        div(
          div(Styles.base,
            div(Styles.header,
              div(Styles.headerLeft,
                div(Styles.number,
                  props.model.content.length.toString()),
                div(Styles.title,
                  a(props.model.title, titleLink)
                )
              ),
              button(Styles.filterButton,
                onClick --> $.modState(_.toggleExpanded),
                i(buttonStyleForState(state): _*)
              )
            ),
            Styles.animationGroup(
              state.expanded ?? makeContent(props.model.content): _*
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
  }

}
