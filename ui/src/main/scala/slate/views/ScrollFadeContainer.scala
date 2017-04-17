package slate
package views

import japgolly.scalajs.react.ReactAddons
import japgolly.scalajs.react.component.Js
import japgolly.scalajs.react.raw.ReactCSSTransitionGroupProps
import japgolly.scalajs.react.vdom.VdomElement

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.scalajs.js
import scalacss.Defaults._
import scalacss.StyleA

abstract class ReactAnimationStyles(val className: String)(implicit r: scalacss.internal.mutable.Register) extends StyleSheet.Inline()(r) {
  def enterClassName: String = className + "-enter"

  def leaveClassName: String = className + "-leave"

  def enterActiveClassName: String = className + "-enter-active"

  def leaveActiveClassName: String = className + "-leave-active"

  val enter: StyleA
  val leave: StyleA
  val enterActive: StyleA
  val leaveActive: StyleA

  def apply(children: VdomElement*): Js.Unmounted[ReactCSSTransitionGroupProps, Null] =
    ReactAddons.CSSTransitionGroup(
      js.Dynamic.literal(transitionName = "className").asInstanceOf[ReactCSSTransitionGroupProps]
    )(children: _*)
}

class ScrollFadeContainer(className: String)(implicit r: scalacss.internal.mutable.Register)
  extends ReactAnimationStyles(className)(r) {

  import dsl._

  val enter: StyleA = style(enterClassName)(
    maxHeight.`0`,
    opacity(0)
  )

  val leave: StyleA = style(leaveClassName)(
    maxHeight(800 px),
    opacity(100)
  )

  val enterActive: StyleA = style(enterActiveClassName)(
    maxHeight(800 px),
    opacity(100),
    transitionProperty := "maxHeight opacity",
    transitionDuration(400.millis),
    transitionTimingFunction.easeInOut
  )

  val leaveActive: StyleA = style(leaveActiveClassName)(
    maxHeight.`0`,
    opacity(0),
    transitionProperty := "maxHeight opacity",
    transitionDuration(400.millis),
    transitionTimingFunction.easeInOut
  )

}

