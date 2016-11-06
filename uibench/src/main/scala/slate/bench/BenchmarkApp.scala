package slate
package bench

import japgolly.scalajs.benchmark.gui.BenchmarkGUI
import org.scalajs.dom
import qq.cc.CompiledFilter
import qq.Platform.Rec._
import qq.data.JSON

import scala.scalajs.js.annotation.JSExport


@JSExport
object BenchmarkApp extends scalajs.js.JSApp {

  import japgolly.scalajs.benchmark.gui.MenuComp.LayoutCfg
  import japgolly.scalajs.react._
  import vdom.prefix_<^._

  //  import demo.Util._
  def configureLayout: LayoutCfg = {
    def top(view: ReactElement): ReactElement =
      <.main(
        <.h1(^.marginBottom := "0.2em", "Benchmark Collection"),
        <.main(^.marginTop := "2.6em", view))
    LayoutCfg.default.copy(topPage = top)
  }

  case class InputParams(size: Int)

  abstract class QQRuntimeParams {
    val iso: String => JSON
  }

  case object QQRuntimeParams extends QQRuntimeParams {
    type T = qq.data.JSON
    override val iso: (String) => T = qq.Json.stringToJSON(_).getOrElse(???)
  }

  abstract class BenchParams {
    val filt: CompiledFilter
    val in: JSON
  }

  object BenchParams {
    def apply(filt0: CompiledFilter, in0: JSON): BenchParams = new BenchParams {
      override val filt: CompiledFilter = filt0
      override val in: JSON = in0
    }
  }

  @JSExport
  def main(): Unit = {
    val body = dom.document getElementById "container"
    BenchmarkGUI.renderMenu(body, layout = configureLayout)(
      CompilerBench.qqCompilerSuite,
      RuntimeBench.qqRuntimeSuite,
      UIBench.startupSuite,
      SerializationBench.serializationBenchSuite
    )
  }

}
