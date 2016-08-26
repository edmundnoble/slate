package dash.bench

import dash.LoggerFactory
import japgolly.scalajs.benchmark.Suite
import japgolly.scalajs.benchmark.gui.{BenchmarkGUI, GuiSuite}
import org.scalajs.dom

import scala.scalajs.js.annotation.JSExport

@JSExport
object BenchmarkApp extends scalajs.js.JSApp {

  private[this] lazy val logger = LoggerFactory.getLogger("BenchmarkApp")

  lazy val suite = GuiSuite(
    Suite("Example Benchmarks")(

    )
  )

  @JSExport
  def main(): Unit = {
    val body = dom.document getElementById "container"
    BenchmarkGUI.renderSuite(body)(suite)
  }

}
