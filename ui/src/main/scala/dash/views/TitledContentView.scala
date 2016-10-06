package dash
package views

import dash.models.TitledContentModel
import japgolly.scalajs.react.{ReactComponentB, _}
import japgolly.scalajs.react.extra.Reusability
import scalaz.syntax.std.boolean._
import Util._

import scalacss.Defaults._

object TitledContentView {

  object Styles extends StyleSheet.Inline {

    import dsl._
    import dash.views.Styles._

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
      addClassName("mdl-list__item-text-body"),
      marginTop(5 px),
      fontFamily(sanFrancisco),
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
      fontFamily(sanFrancisco),
      whiteSpace nowrap,
      overflow.hidden.important,
      position relative,
      marginBottom(5 px)
    )

    val titleFade = style(
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

  }

  def builder: ReactComponentB[TitledContentModel, Unit, Unit, TopNode] = {
    import japgolly.scalajs.react.vdom.all._

    import scalacss.ScalaCssReact._

    ReactComponentB[TitledContentModel]("Issue")
      .renderP((_, model) =>
        li(Styles.base, key := model.title,
          model.content.isEmpty !? (Styles.tallCell: TagMod),
          div(model.content.isEmpty ?? (Styles.tall: TagMod),
            span(Styles.title,
              a(Styles.title,
                model.title,
                href := model.titleUrl
              )
            ),
            model.content.isEmpty !?
              (div(Styles.content,
                model.content): TagMod)
          )
        )
      )
      .configure(Reusability.shouldComponentUpdate)
  }

}
