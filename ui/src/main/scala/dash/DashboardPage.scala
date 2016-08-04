package dash

import dash.models.ExpandableContentModel
import dash.views.ReactiveReact.ReactiveState
import dash.views.{ExpandableContentView, Styles}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import monix.execution.Scheduler
import monix.reactive.Observable
import qq.Util._
import dash.Util._
import japgolly.scalajs.react.vdom.ReactNodeFrag

import scalacss.Defaults._

object DashboardPage {

  import japgolly.scalajs.react.vdom.all._

  import scala.language.implicitConversions
  import scalacss.ScalaCssReact._

  def makeFilterRow(results: Seq[ExpandableContentModel]): ReactElement = {
    def toView(r: ExpandableContentModel) = ExpandableContentView.builder.build(r)
    div(Styles.filterContainer,
      results.map(result =>
        div(Styles.innerFilterContainer,
          toView(result)
        )
      )
    )
  }

  case class SearchPageState(expandableContentModels: IndexedSeq[ExpandableContentModel])
    extends ReactiveState[SearchPageState, IndexedSeq[ExpandableContentModel], Unit](expandableContentModels, ()) {
    override def setReactive(r: IndexedSeq[ExpandableContentModel]): SearchPageState = copy(expandableContentModels = r)
  }

  object SearchPageState {
    implicit val reusability = Reusability.byRefOr_==[SearchPageState]
  }

  def makeSearchPage(expandableContentModelStream: Observable[IndexedSeq[ExpandableContentModel]])(implicit sch: Scheduler): ReactElement = {
    import views.ReactiveReact._
    ReactComponentB[Observable[IndexedSeq[ExpandableContentModel]]]("Main search page")
      .initialState(SearchPageState(IndexedSeq.empty))
      .renderS { (_, state) =>
        div(Styles.render[ReactElement],
          id := "react-root",
          div(Styles.appBar,
            table(
              tr(Styles.appBarRow,
                td(Styles.appBarText,
                  "Dashboarder"
                )
              )
            )
          ),
          div(
            div(
              (Styles.container: TagMod) +:
              state.expandableContentModels.grouped(2).map(makeFilterRow(_): TagMod).toSeq: _*
            )
          )
        ).render
      }
      .configure(Reusability.shouldComponentUpdate)
      .reactiveReplace
      .build(expandableContentModelStream)
  }

}

object LinkStyles extends StyleSheet.Standalone {

  import dsl._

  "a" - color(c"#616161")
}
