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

  val materialBlue: Color = c"#2196F3"
  val materialGrey: Color = c"#757575"

  val pseudoContent: AV = content := "\"\""

  val appBar: StyleA = style(
    addClassName("mui-appbar")
  )

  val appBarRow: StyleA = style(
    verticalAlign.middle
  )

  val appBarText: StyleA = style(
    addClassNames("mui--appbar-height", "mui--text-display1"),
    paddingLeft(10 px),
    fontFamily(sanFranciscoLight)
  )

  val innerFilterContainer: StyleA = style(
    addClassName("mui-col-md-6")
  )

  val filterContainer: StyleA = style(
    addClassName("mui-row")
  )

  val container: StyleA = style(
    addClassName("mui-container-fluid"),
    width(100 %%),
    fontFamily(sanFrancisco),
    color(materialBlue),
    marginTop(30 px)
  )

  val dashboardContainer: ScrollFadeContainer = new ScrollFadeContainer("dashboardContainer")

}
