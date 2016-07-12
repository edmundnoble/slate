package dash.views

import japgolly.scalajs.react.Addons.ReactCssTransitionGroup
import japgolly.scalajs.react.{ReactComponentU_, ReactNode}

import scala.concurrent.duration._
import scalacss.Defaults._
import scalacss.{FontFace, NonEmptyVector, StyleA}

object Styles extends StyleSheet.Inline {

  import dsl._

  import scala.language.postfixOps

  val roboto = FontFace("Roboto", src = NonEmptyVector("fonts/roboto-v15-latin-ext_latin-300.woff2"))
  val robotoLight = FontFace("Roboto", src = NonEmptyVector("fonts/roboto-v15-latin-ext_latin-500.woff2"))
  val robotoMedium = FontFace("Roboto", src = NonEmptyVector("fonts/roboto-v15-latin-ext_latin-700.woff2"))
  val robotoHeavy = FontFace("Roboto", src = NonEmptyVector("fonts/roboto-v15-latin-ext_latin-900.woff2"))

  val materialBlue = c"#2196F3"
  val materialGrey = c"#757575"

  val pseudoContent = content := "\"\""

  val appBar = style(
    addClassName("mui-appbar")
  )

  val appBarRow = style(
    verticalAlign.middle
  )

  val appBarText = style(
    addClassNames("mui--appbar-height", "mui--text-display1"),
    paddingLeft(10 px),
    fontFamily(robotoLight)
  )

  val innerFilterContainer = style(
    addClassName("mui-col-md-6")
  )

  val filterContainer = style(
    addClassName("mui-row")
  )

  val container = style(
    addClassName("mui-container-fluid"),
    width(100 %%),
    fontFamily(roboto),
    color(materialBlue),
    marginTop(30 px)
  )

  abstract class ReactAnimationStyles(val className: String)(implicit r: scalacss.mutable.Register) extends StyleSheet.Inline()(r) {
    def enterClassName = className + "-enter"
    def leaveClassName = className + "-leave"
    def enterActiveClassName = className + "-enter-active"
    def leaveActiveClassName = className + "-leave-active"
    val enter: StyleA
    val leave: StyleA
    val enterActive: StyleA
    val leaveActive: StyleA
    def apply(children: ReactNode*): ReactComponentU_ = ReactCssTransitionGroup(className)(children)
  }

  class ScrollFadeContainer(className: String)(implicit r: scalacss.mutable.Register) extends ReactAnimationStyles(className)(r) {

    val enter: StyleA = style(enterClassName)(
      maxHeight.`0`,
      opacity(0)
    )

    val leave: StyleA = style(leaveClassName)(
      maxHeight(1200 px),
      opacity(100)
    )

    val enterActive: StyleA = style(enterActiveClassName)(
      maxHeight(1200 px),
      opacity(100),
      transitionProperty := "maxHeight opacity",
      transitionDuration(300.millis),
      transitionTimingFunction.easeInOut
    )

    val leaveActive: StyleA = style(leaveActiveClassName)(
      maxHeight.`0`,
      opacity(0),
      transitionProperty := "maxHeight opacity",
      transitionDuration(300.millis),
      transitionTimingFunction.easeInOut
    )

  }

}
