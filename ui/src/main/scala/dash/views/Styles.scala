package dash
package views

import scalacss.Defaults._
import scalacss.StyleA
import scalacss.internal.Macros.Color
import scalacss.internal.{AV, FontFace, NonEmptyVector}

object Styles extends StyleSheet.Inline {

  import dsl._

  import scala.language.postfixOps

  val sanFrancisco: FontFace = FontFace("San Francisco", src = NonEmptyVector("fonts/sanfrancisco/sanfranciscodisplay-regular-webfont.woff"))
  val sanFranciscoLight: FontFace = FontFace("San Francisco", src = NonEmptyVector("fonts/sanfrancisco/sanfranciscodisplay-thin-webfont.woff"))
  val sanFranciscoMedium: FontFace = FontFace("San Francisco", src = NonEmptyVector("fonts/sanfrancisco/sanfranciscodisplay-medium-webfont.woff"))
  val sanFranciscoSubheavy: FontFace = FontFace("San Francisco", src = NonEmptyVector("fonts/sanfrancisco/sanfranciscodisplay-semibold-webfont.woff"))
  val sanFranciscoHeavy: FontFace = FontFace("San Francisco", src = NonEmptyVector("fonts/sanfrancisco/sanfranciscodisplay-bold-webfont.woff"))

  val akrobat: FontFace = FontFace("Akrobat", src = NonEmptyVector("fonts/akrobat/AkrobatRegular.woff2"))
  val akrobatThin: FontFace = FontFace("Akrobat", src = NonEmptyVector("fonts/akrobat/AkrobatThin.woff2"))
  val akrobatMedium: FontFace = FontFace("Akrobat", src = NonEmptyVector("fonts/akrobat/AkrobatMedium.woff2"))
  val akrobatSemibold: FontFace = FontFace("Akrobat", src = NonEmptyVector("fonts/akrobat/AkrobatSemiBold.woff2"))
  val akrobatBlack: FontFace = FontFace("Akrobat", src = NonEmptyVector("fonts/akrobat/AkrobatBlack.woff2"))
  val akrobatBold: FontFace = FontFace("Akrobat", src = NonEmptyVector("fonts/akrobat/AkrobatBold.woff2"))

  val materialBlue: Color = c"#2196F3"
  val materialGrey: Color = c"#757575"
  val materialGrays: Color = c"#BDBDBD"

  val pseudoContent: AV = content := "\"\""

  val appBarText: StyleA = style(
    color(c"#303030"),
    fontSize(160 %%),
    letterSpacing(2 px),
    fontWeight._800,
    paddingLeft(75 px),
    fontFamily(akrobatBlack)
  )

  val container: StyleA = style(
    addClassNames("mdl-grid", "page-content"),
    width(90 %%),
    marginTop(25 px)
  )

  val dashboardContainer: ScrollFadeContainer = new ScrollFadeContainer("dashboardContainer")

}
