package slate
package views

import japgolly.scalajs.react.component.ScalaBuilder
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react._
import slate.models.TitledContentModel

import scalacss.Defaults._

object TitledContentView {

  object Styles extends StyleSheet.Inline {

    import dsl._
    import slate.views.Styles._

    import scala.language.postfixOps

    val base = style(
      addClassNames("mdl-list__item"),
      padding(6 px).important,
      maxHeight(4.5 em).important,
      minHeight(1 em).important,
      pageBreakInside avoid,
      position relative
    )

    val tallCell = style(
      addClassName("mdl-list__item--three-line")
    )

    val tall = style(
      addClassName("mdl-list__item-primary-content")
    )

    val fade = style(
      &.after.apply(
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
      addClassName("mdl-list__item-text-body"),
      marginTop(5 px),
      fontFamily :=! "San Francisco",
      overflow hidden,
      maxHeight(2.4 em),
      minHeight(2.4 em),
      fontSize(14 px)
    )

    val title = style(
      width(100 %%),
      //      addClassName("mdl-typography--headline"),
      color(rgba(0, 0, 0, 0.86)),
      (textOverflow := "ellipsis").important,
      fontSize(17 px),
      fontFamily :=! "San Francisco",
      whiteSpace nowrap,
      overflow.hidden.important,
      position relative,
      marginBottom(5 px)
    )

    val titleFade = style(
      &.after.apply(
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

  }

  def builder: ScalaBuilder.Step4[TitledContentModel, Children.None, Unit, Unit] = {
    import japgolly.scalajs.react.vdom.all._

    import scalacss.ScalaCssReact._

    ScalaComponent.builder[TitledContentModel]("Issue")
      .renderP((_, model) =>
        li(Styles.base, key := model.title,
          Styles.tallCell.when(model.content.nonEmpty),
          div(Styles.tall.when(model.content.isEmpty),
            span(Styles.title,
              a(Styles.title,
                model.title,
                href :=? model.titleUrl
              )
            ),
            div(Styles.content,
              model.content).when(model.content.nonEmpty)
          )
        )
      )
      .configure(Reusability.shouldComponentUpdate)
  }

}
