package slate
package views

import scalacss.Defaults._
import scalacss.StyleA
import scalacss.internal.Macros.Color
import scalacss.internal.{AV, FontFace, NonEmptyVector}

object Styles extends StyleSheet.Inline {

  import dsl._

  import scala.language.postfixOps

  val sanFrancisco: FontFace = FontFace("San Francisco", src =
    NonEmptyVector(
      "fonts/sanfrancisco/sanfranciscodisplay-regular-webfont.woff",
      "fonts/sanfrancisco/sanfranciscodisplay-thin-webfont.woff",
      "fonts/sanfrancisco/sanfranciscodisplay-medium-webfont.woff",
      "fonts/sanfrancisco/sanfranciscodisplay-semibold-webfont.woff",
      "fonts/sanfrancisco/sanfranciscodisplay-bold-webfont.woff"
    )
  )

  val akrobat: FontFace = FontFace("Akrobat", src =
    NonEmptyVector(
      "fonts/akrobat/AkrobatRegular.woff2",
      "fonts/akrobat/AkrobatThin.woff2",
      "fonts/akrobat/AkrobatMedium.woff2",
      "fonts/akrobat/AkrobatSemiBold.woff2",
      "fonts/akrobat/AkrobatBlack.woff2",
      "fonts/akrobat/AkrobatBold.woff2"
    )
  )

  val materialBlue: Color = c"#2196F3"
  val materialGrey: Color = c"#757575"
  val materialGrays: Color = c"#BDBDBD"

  val pseudoContent: AV = content := "\"\""

  val appBarText: StyleA = style(
    addClassName("mdl-layout-title"),
    color(c"#303030"),
    fontSize(160 %%),
    letterSpacing(2 px),
    fontWeight._800,
    paddingLeft(75 px),
    fontFamily(akrobat)
  )

  val layoutContainer: StyleA = style(
    addClassName("mdl-layout__container")
  )

  val layoutContent = style(
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
