package dash.views

import dash.views
import japgolly.scalajs.react._

import scalacss.Defaults._

object ErrorView {
  object Styles extends StyleSheet.Inline {

    import dsl._

    val danger: StyleA = style(
      margin(20 px),
      fontFamily(views.Styles.sanFranciscoMedium)
    )
  }

  import vdom.all._

  import scalacss.ScalaCssReact._

  def builder: ReactComponentB[Throwable, Unit, Unit, TopNode] =
    ReactComponentB[Throwable]("errorView").stateless.noBackend.render_P { ex =>
      div(Styles.danger, ex.toString())
    }


}
