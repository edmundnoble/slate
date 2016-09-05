package dash.bench

import dash.app.DashboarderApp
import japgolly.scalajs.benchmark.{Benchmark, Suite}
import japgolly.scalajs.benchmark.gui.GuiSuite
import monix.reactive.Observable
import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.raw.HTMLDivElement

object UIBench {

  val startupSuite = GuiSuite(
    Suite("UI Startup Benchmarks")(
      Benchmark("UI empty load time") {
        DashboarderApp.render(dom.document.createElement("div"), Observable.never, Observable.never)
      }
    )
  )

}
