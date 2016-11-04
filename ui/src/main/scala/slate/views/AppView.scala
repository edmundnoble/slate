package slate
package views

import slate.models.AppModel
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.{ReactComponentB, ReactNode, TopNode}
import monix.execution.Scheduler
import slate.Util.observableReusability
import slate.views.ExpandableContentView.ExpandableContentProps

import scalacss.Defaults._
import cats.data.Xor
import cats.implicits._

object AppView {

  object Styles extends StyleSheet.Inline {

    import dsl._
    import scala.language.postfixOps

    val panel = style(
      addClassNames("mdl-cell", "mdl-card", "mdl-shadow--2dp", "mdl-cell--6-col", "mdl-color--grey-100", "mdl-color-text--grey-600"),
      marginRight(10 px),
      marginLeft(10 px),
      paddingBottom(20 px),
      paddingLeft(-5 px),
      width :=! "calc(50% - 20px)"
    )

    val header = style(
      addClassName("mdl-card__title"),
      marginTop(-10 px),
      display.inlineBlock
    )

    val linkIcon = style(
      color(c"#303030"),
      addClassName("material-icons"),
      fontSize(24 px),
      marginLeft(6 px),
      marginTop(-7 px),
      verticalAlign.middle,
      (textDecoration := "none").important
    )

    val title = style(
      addClassNames("mdl-card__title-text"),
      color(c"#303030"),
      (textDecoration := "none").important,
      fontWeight._700,
      textOverflow := "ellipsis",
      letterSpacing(1 px),
      fontFamily(slate.views.Styles.akrobat),
      display inline
    )

    val content = style(
      width(100 %%),
      overflow.hidden
    )

    val animationGroup = new slate.views.ScrollFadeContainer("filter-group")

  }

  final case class AppState(model: AppModel)

  object AppState {
    implicit val reusability: Reusability[AppState] =
      Reusability.caseClass[AppState]
  }

  final case class AppProps(id: Int, title: String, titleLink: String, model: AppModel)

  object AppProps {
    implicit val reusability: Reusability[AppProps] =
      Reusability.byRefOr_==
  }

  def builder(implicit sch: Scheduler): ReactComponentB[AppProps, AppState, Unit, TopNode] = {
    import japgolly.scalajs.react.vdom.all._

    import scalacss.ScalaCssReact._

    ReactComponentB[AppProps]("Expandable content view")
      .initialState[AppState](AppState(AppModel(Nil.right)))
      .renderP { (_, props) =>
        div(Styles.panel, key := props.id,
          div(Styles.header,
            div(ExpandableContentView.Styles.headerLeft,
              span(Styles.title, props.title.toUpperCase()),
              a(Styles.linkIcon, target := "_blank", "link", href := props.titleLink)
            )
          ),
          div(Styles.content,
            Styles.animationGroup(
              props.model.content.fold[List[ReactNode]]({ ex =>
                ErrorView.builder(ex) :: Nil
              }, _.map { model =>
                ExpandableContentView.builder.build(ExpandableContentProps(model, initiallyExpanded = false))
              }): _*
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
  }


}
