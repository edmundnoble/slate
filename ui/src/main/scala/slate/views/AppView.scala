package slate
package views

import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.{ReactComponentB, ReactNode, TopNode}
import monix.execution.Scheduler
import qq.data.JSON
import slate.app.SlateApp.AllErrors
import slate.models.AppModel
import slate.views.ExpandableContentView.ExpandableContentProps

import scala.scalajs.js
import scalacss.Defaults._

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
      fontFamily :=! "Akrobat",
      display inline
    )

    val headerRightDate = style(
      fontSize(10 px),
      marginRight(10 px),
      float right,
      marginTop(10 px),
      marginBottom(10 px)
    )

    val content = style(
      width(100 %%),
      overflow.hidden
    )

    val animationGroup = new slate.views.ScrollFadeContainer("filter-group")

  }

  implicit def dateReusability: Reusability[js.Date] =
    Reusability.double(0).contramap(_.getTime())

  implicit val errorReusability: Reusability[AllErrors] =
    Reusability.byRef

  final case class AppProps(id: Int, input: JSON, title: String, titleLink: String, model: Option[AllErrors Either AppModel])

  object AppProps {
    implicit val reusability: Reusability[AppProps] =
      Reusability.byRefOr_==
  }

  // TODO: remove from here and SlatePrelude
  def formatDatetimeFriendlyImpl(d: js.Date): String = {
    val diff = js.Date.now() - d.getTime()
    val deltaSeconds = Math.round(diff / 1000)

    val minute = 60
    val hour = minute * 60
    val day = hour * 24
    val week = day * 7

    if (deltaSeconds < minute) {
      "just now"
    } else if (deltaSeconds < 2 * minute) {
      "a minute ago"
    } else if (deltaSeconds < hour) {
      Math.floor(deltaSeconds / minute).toString + " minutes ago"
    } else if (Math.floor(deltaSeconds / hour) == 1) {
      "1 hour ago"
    } else if (deltaSeconds < day) {
      Math.floor(deltaSeconds / hour).toString + " hours ago"
    } else if (deltaSeconds < day * 2) {
      "tomorrow"
    } else if (deltaSeconds < week) {
      Math.floor(deltaSeconds / day) + " days ago"
    } else {
      Math.floor(deltaSeconds / week) + " weeks ago"
    }
  }

  def builder(implicit sch: Scheduler): ReactComponentB[AppProps, Unit, Unit, TopNode] = {
    import japgolly.scalajs.react.vdom.all._

    import scalacss.ScalaCssReact._

    ReactComponentB[AppProps]("Expandable content view")
      .stateless
      .renderP { (_, props) =>
        div(Styles.panel, key := props.id,
          div(Styles.header,
            div(ExpandableContentView.Styles.headerLeft,
              span(Styles.title, props.title.toUpperCase()),
              a(Styles.linkIcon, target := "_blank", "link", href := props.titleLink)
            ),
            props.model.flatMap(_.right.toOption).map(mo =>
              div(Styles.headerRightDate,
                "last updated " + formatDatetimeFriendlyImpl(mo.updated)
              )
            )
          ),
          div(Styles.content,
            Styles.animationGroup(
              props.model.fold[List[ReactNode]](
                LoadingView.builder() :: Nil
              )(m => m.fold[List[ReactNode]]({ ex =>
                ErrorView.builder(ex) :: Nil
              }, appModel => appModel.content.map { model =>
                ExpandableContentView.builder.build(ExpandableContentProps(model, initiallyExpanded = false))
              })): _*
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
  }


}
