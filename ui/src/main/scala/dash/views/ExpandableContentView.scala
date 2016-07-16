package dash.views

import dash.models.ExpandableContentModel
import dash.views.ReactiveReact.ReactiveState
import japgolly.scalajs.react._
import monix.execution.Scheduler
import monix.reactive.Observable
import monocle.Lens
import monocle.macros.GenLens

import scalacss.Defaults._

object ExpandableContentView {
  case class ExpandableState(expanded: Boolean)

  object ExpandableState {
    def expanded = GenLens[ExpandableState](_.expanded)
  }

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
      addClassName("mui-panel"),
      overflow.hidden,
      marginBottom(40 px)
    )

    val header = style(
      addClassName("mui--divider-bottom"),
      width(100 %%),
      display.inlineBlock
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
      addClassName("mui--text-headline"),
      paddingRight(10 px),
      display inline
    )

    val title = style(
      addClassName("mui--text-headline"),
      fontFamily(robotoHeavy),
      display inline
      //fontSize(150 %%)
    )

    val animationGroup = new dash.views.Styles.ScrollFadeContainer("filter-group")

  }

  def build(props: ExpandableContentModel): ReactComponentU[ExpandableContentModel, ExpandableState, Unit, TopNode] = {
    import MonocleReact._
    import japgolly.scalajs.react.vdom.prefix_<^._

    import scalacss.ScalaCssReact._

    def buttonStyleForState(state: ExpandableState): Seq[TagMod] = {
      val otherStyles = Vector[TagMod](Styles.filterButtonIcon, "expand_more")
      if (state.expanded) otherStyles.:+[TagMod, Vector[TagMod]](Styles.filterButtonExpanded)
      else otherStyles
    }

    ReactComponentB[ExpandableContentModel]("Expandable content view")
      .initialState(ExpandableState(expanded = false))
      .renderPS { ($, model, state) =>
        val titleLink = ^.href := model.titleUrl
        <.div(Styles.render[ReactElement],
          <.div(Styles.base,
            <.div(Styles.header,
              <.div(Styles.headerLeft,
                <.div(Styles.number,
                  <.a(model.content.length.toString(), titleLink)),
                <.div(Styles.title,
                  <.a(model.title, titleLink)
                )
              ),
              <.button(Styles.filterButton,
                ^.onClick --> $.modStateL(ExpandableState.expanded)(!_),
                <.i(buttonStyleForState(state): _*)
              )
            ),
            Styles.animationGroup(
              (if (state.expanded) {
                model.content.map(TitledContentView.build)
              } else {
                Vector.empty[ReactElement]
              }): _*
            )
          )
        )
      }
      .domType[TopNode]
      .build(props)
  }

}
