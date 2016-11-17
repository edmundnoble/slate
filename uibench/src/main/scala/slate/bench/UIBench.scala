package slate
package bench

import japgolly.scalajs.benchmark.gui.GuiSuite
import japgolly.scalajs.benchmark.{Benchmark, Suite}
import org.scalajs.dom
import slate.app.SlateApp
import slate.views.DashboardPage.SearchPageProps

object UIBench {

  import monix.execution.Scheduler.Implicits.global

  val startupSuite = GuiSuite(
    Suite("UI Startup Benchmarks")(
      Benchmark("UI empty load time") {
        SlateApp.render(dom.document.createElement("div"), SearchPageProps(Nil))
      }
    )
  )

}
