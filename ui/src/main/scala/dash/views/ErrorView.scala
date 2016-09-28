package dash.views

import dash.views
import japgolly.scalajs.react._

import scalacss.Defaults._

object ErrorView {
  object Styles extends StyleSheet.Inline {

    import dsl._

    val danger: StyleA = style(
      fontFamily(views.Styles.sanFranciscoMedium),
      addClassName("mui--text-danger")
    )
  }

  import vdom.all._

  import scalacss.ScalaCssReact._

  def builder: ReactComponentB[Throwable, Unit, Unit, TopNode] =
    ReactComponentB[Throwable]("errorView").stateless.noBackend.render_P { ex =>
      div(Styles.danger, ex.getMessage())
    }


}
