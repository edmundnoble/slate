package dash.views

import dash.models.ExpandableContentModel
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import monix.execution.Scheduler
import monix.reactive.Observable
import monocle.macros.GenLens
import dash.Util._

import scala.language.higherKinds
import scalaz.syntax.std.boolean._
import scalaz.std.list._
import scalacss.Defaults._

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

  case class ExpandableState(expanded: Boolean, model: ExpandableContentModel)

  object ExpandableState {
    val expanded = GenLens[ExpandableState](_.expanded)
    implicit val reusability: Reusability[ExpandableState] =
      Reusability.caseClass[ExpandableState]
  }

  case class ExpandableContentProps(models: Observable[ExpandableContentModel], expanded: Boolean) {
  }

  object ExpandableContentProps {
    implicit val reusability: Reusability[ExpandableContentProps] =
      Reusability.caseClass[ExpandableContentProps]
  }

  def builder(implicit sch: Scheduler
             ): ReactComponentB[ExpandableContentProps, ExpandableState, Unit, TopNode] = {
    import MonocleReact._
    import japgolly.scalajs.react.vdom.all._
    import dash.views.ReactiveReact._

    import scalacss.ScalaCssReact._

    def buttonStyleForState(state: ExpandableState): Seq[TagMod] = {
      val otherStyles = Vector[TagMod](Styles.filterButtonIcon, "expand_more")
      if (state.expanded) otherStyles :+ styleaToTagMod(Styles.filterButtonExpanded)
      else otherStyles
    }

    ReactComponentB[ExpandableContentProps]("Expandable content view")
      .initialState[ExpandableState](ExpandableState(expanded = false, ExpandableContentModel("", None, Nil)))
      .renderS { ($, state) =>
        val titleLink = href := state.model.titleUrl
        div(
          div(Styles.base,
            div(Styles.header,
              div(Styles.headerLeft,
                div(Styles.number,
                  a(state.model.content.length.toString(), titleLink)),
                div(Styles.title,
                  a(state.model.title, titleLink)
                )
              ),
              button(Styles.filterButton,
                onClick --> $.modStateL(ExpandableState.expanded)(!_),
                i(buttonStyleForState(state): _*)
              )
            ),
            Styles.animationGroup(
              state.expanded ?? state.model.content.map(TitledContentView.builder.build(_)): _*
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .reactiveReplaceL[ExpandableContentModel](_.models, (st, mod) => st.copy(model = mod))
  }

}
