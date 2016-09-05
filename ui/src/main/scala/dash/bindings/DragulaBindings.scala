package dash
package bindings

import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.{Array, UndefOr}

@js.native
trait DragulaOptions extends js.Object {

  // function (el)
  @js.native
  def isContainer: UndefOr[HTMLElement => Boolean] = js.native

  // function (el, source, handle, sibling)
  @js.native
  def moves: UndefOr[(HTMLElement, HTMLElement, HTMLElement, HTMLElement) => Boolean] = js.native

  // function (el, target, source, sibling)
  @js.native
  def accepts: UndefOr[(HTMLElement, HTMLElement, HTMLElement, HTMLElement) => Boolean] = js.native

  // function (el, handle)
  @js.native
  def invalid: UndefOr[(HTMLElement, HTMLElement) => Boolean] = js.native

  @js.native
  def copy: UndefOr[Boolean] = js.native

  @js.native
  def copySortSource: UndefOr[Boolean] = js.native

  @js.native
  def revertOnSpill: UndefOr[Boolean] = js.native

  @js.native
  def removeOnSpill: UndefOr[Boolean] = js.native

  @js.native
  def direction: UndefOr[HTMLElement] = js.native

  @js.native
  def ignoreInputTextSelection: UndefOr[Boolean] = js.native

  @js.native
  def mirrorContainer: UndefOr[HTMLElement] = js.native
}

@js.native
trait Drake extends js.Object {
  @js.native
  def containers: UndefOr[js.Array[HTMLElement]] = js.native

  @js.native
  def dragging: Boolean = js.native

  @js.native
  def start(item: HTMLElement): Unit = js.native

  @js.native
  def end(): Unit = js.native

  @js.native
  def cancel(revert: Boolean): Unit = js.native

  @js.native
  def remove(): Unit = js.native

  @js.native
  def on() = js.native

}

@js.native
@JSName("dragula")
object Dragula extends js.Function2[js.Array[HTMLElement], DragulaOptions, Drake] {
  @js.native
  override def apply(v1: Array[HTMLElement], v2: DragulaOptions): Drake = js.native
}
