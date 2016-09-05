package dash
package views

import dash.models.TitledContentModel
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.ReactComponentB.PSB

import scalacss.Defaults._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability

object TitledContentView {

  object Styles extends StyleSheet.Inline {

    import dsl._
    import scala.language.postfixOps
    import dash.views.Styles._

    val base = style(
      display contents,
      pageBreakInside avoid,
      position relative,
      marginBottom(10 px),
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
      addClassName("mui--text-title"),
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
            div(Styles.title,
              a(
                model.title,
                href := model.titleUrl
              )
            ),
            div(Styles.content,
              model.content
            )
          )
        )
      )
      .configure(Reusability.shouldComponentUpdate)
  }

}
