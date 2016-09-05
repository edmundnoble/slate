package dash.bench

import dash.app.DashboarderApp
import japgolly.scalajs.benchmark.gui.GuiSuite
import japgolly.scalajs.benchmark.{Benchmark, Suite}
import monix.reactive.Observable
import org.scalajs.dom

object UIBench {

  val startupSuite = GuiSuite(
    Suite("UI Startup Benchmarks")(
      Benchmark("UI empty load time") {
        DashboarderApp.render(dom.document.createElement("div"), Observable.never, Observable.never)
      }
    )
  )

}
