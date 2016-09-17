package dash
package bench

import japgolly.scalajs.benchmark.gui.BenchmarkGUI
import org.scalajs.dom
import qq.QQCompiler.CompiledFilter
import qq._
import qq.cc.{QQRuntime, UpickleRuntime}
import qq.jsc.JSRuntime
import upickle.Js

import scala.scalajs.js.JSON
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
    type T
    val runtime: QQRuntime[T]
    val iso: String => T
  }

  case object JSRuntimeParams extends QQRuntimeParams {
    type T = Any
    override val runtime: QQRuntime[Any] = JSRuntime
    override val iso: (String) => Any = JSON.parse(_)
  }
  case object UpickleRuntimeParams extends QQRuntimeParams {
    type T = Js.Value
    override val runtime: QQRuntime[Js.Value] = UpickleRuntime
    override val iso: (String) => Js.Value = upickle.json.read
  }

  abstract class BenchParams {
    type T
    val filt: CompiledFilter[T]
    val in: T
  }

  object BenchParams {
    def apply[T0](filt0: CompiledFilter[T0], in0: T0): BenchParams {type T = T0} = new BenchParams {
      override type T = T0
      override val filt: CompiledFilter[T0] = filt0
      override val in: T0 = in0
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
