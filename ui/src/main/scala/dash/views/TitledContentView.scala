package dash
package views

import dash.models.TitledContentModel
import japgolly.scalajs.react.{ReactComponentB, _}
import japgolly.scalajs.react.extra.Reusability

import scalacss.Defaults._

object TitledContentView {

  object Styles extends StyleSheet.Inline {

    import dsl._
    import dash.views.Styles._

    import scala.language.postfixOps

    val base = style(
      display contents,
      pageBreakInside avoid,
      position relative,
      margin(10 px)
    )

    val fade = style(
      &.after(
        height(1.2 em),
        bottom(0 px),
        position absolute,
        textAlign right,
        pseudoContent,
        width(100 %%),
        backgroundImage := "linear-gradient(rgba(255, 255, 255, 0), rgba(255, 255, 255, 1))"
      )
    )

    val content = style(
      addClassName("mui--text-body1"),
      color(black),
      overflow hidden,
      maxHeight(3.2 em),
      minHeight(2.4 em)
    )

    val title = style(
      addClassName("mui--text-subhead"),
      whiteSpace nowrap,
      overflow hidden,
      position relative,
      marginBottom(5 px),
      &.after(
        height(1.2 em),
        bottom(0 px),
        right(0 px),
        position absolute,
        pseudoContent,
        textAlign right,
        backgroundImage := "linear-gradient(left, rgba(255, 255, 255, 0), rgba(255, 255, 255, 1))",
        width(10 %%)
      )
    )

    val filterIssueContainer = new ScrollFadeContainer("filterIssueContainer")
    val searchResultContainer = new ScrollFadeContainer("searchResultContainer")
  }

  def builder: ReactComponentB[TitledContentModel, Unit, Unit, TopNode] = {
    import japgolly.scalajs.react.vdom.all._

    import scalacss.ScalaCssReact._

    ReactComponentB[TitledContentModel]("Issue")
      .renderP((_, model) =>
        div(
          key := model.title + model.titleUrl,
          div(Styles.base,
            if (model.content.isEmpty)
              (Nil: List[TagMod]): TagMod
            else
              Styles.fade,
            div(Styles.title,
              a(
                model.title,
                href := model.titleUrl
              )
            ),
            if (model.content.isEmpty)
              (Nil: List[TagMod]): TagMod
            else
              div(Styles.content,
                model.content)
          )
        )
      )
      .configure(Reusability.shouldComponentUpdate)
  }

}
