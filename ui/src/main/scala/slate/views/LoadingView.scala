package slate.views

import org.scalajs.dom.html.Div
import slate.app.SlateApp.AllErrors

object LoadingView {

  import japgolly.scalajs.react.vdom.all._

  def builder(): ReactTagOf[Div] = {
    div("Loading")
  }

}
