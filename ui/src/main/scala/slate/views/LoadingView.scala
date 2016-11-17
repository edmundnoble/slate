package slate
package views

import org.scalajs.dom.html.Div

object LoadingView {

  import japgolly.scalajs.react.vdom.all._

  def builder(): ReactTagOf[Div] = {
    div("Loading")
  }

}
