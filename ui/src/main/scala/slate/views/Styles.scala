package slate
package views

import scalacss.Defaults._
import scalacss.StyleA
import scalacss.internal.AV
import scalacss.internal.Macros.Color

object Styles extends StyleSheet.Inline {

  import dsl._

  import scala.language.postfixOps

  val materialBlue: Color = c"#2196F3"
  val materialGrey: Color = c"#757575"
  val materialGrays: Color = c"#BDBDBD"

  val pseudoContent: AV = content := "\"\""

  val appBarText: StyleA = style(
    addClassName("mdl-layout-title"),
    color(c"#303030"),
    fontSize(180 %%),
    letterSpacing(2 px),
    fontWeight._800,
    paddingLeft(75 px),
    fontFamily :=! "Akrobat"
  )

  val layoutContainer: StyleA = style(
    addClassName("mdl-layout__container")
  )

  val layoutContent: StyleA = style(
    addClassName("mdl-layout__content"),
    // keep the scroll bar always, so expanding cards doesn't add it in and change the width of the content
    overflowY.scroll.important
  )

  val container: StyleA = style(
    addClassNames("mdl-grid", "page-content"),
    // prevent grid items from expanding their entire grid row at once
    alignItems.flexStart.important,
    width(95 %%),
    marginTop(25 px)
  )

  val dashboardContainer: ScrollFadeContainer = new ScrollFadeContainer("dashboardContainer")

}
