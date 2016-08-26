package dash.bench

import dash.LoggerFactory
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui.{BenchmarkGUI, GuiParam, GuiParams, GuiSuite}
import monix.eval.Task
import monocle.Iso
import monocle.macros.GenIso
import org.scalajs.dom
import qq.QQCompiler.CompiledFilter
import qq.jsc.JSRuntime
import qq._
import upickle.Js
import upickle.Js.Value

import scala.annotation.tailrec
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSExport
import scalaz.std.function._
import scalaz.syntax.all._

@JSExport
object BenchmarkApp extends scalajs.js.JSApp {

  private[this] lazy val logger = LoggerFactory.getLogger("BenchmarkApp")

  import japgolly.scalajs.benchmark.gui.MenuComp.LayoutCfg
  import japgolly.scalajs.react._, vdom.prefix_<^._

  //  import demo.Util._
  def configureLayout: LayoutCfg = {
    def top(view: ReactElement): ReactElement =
      <.main(
        <.h1(^.marginBottom := "0.2em", "Benchmark Collection"),
        <.main(^.marginTop := "2.6em", view))
    LayoutCfg.default.copy(topPage = top)
  }

  @tailrec
  def buildRec(transform: Filter => Filter, count: Int, start: Filter): Filter = {
    if (count == 0) start
    else buildRec(transform, count - 1, transform(start))
  }

  val qqCompilerSuite = GuiSuite(
    Suite("QQ Compiler Benchmarks")(
      Benchmark.setup[Int, Filter] { i =>
        buildRec(FilterDSL.compose(_, FilterDSL.selectKey("key")), i, FilterDSL.id)
      }("select key") { filt =>
        QQCompiler.compile(JSRuntime, Nil, filt)
      },
      Benchmark.setup[Int, Filter] { i =>
        buildRec(FilterDSL.compose(_, FilterDSL.id), i, FilterDSL.id)
      }("compose with id") { filt =>
        QQCompiler.compile(JSRuntime, Nil, filt)
      }
    ), GuiParams.one(GuiParam.int("filter size", 5, 10, 100, 200))
  )

  case class InputParams(size: Int, input: String = "[]")

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

  val qqRuntimeSuite = GuiSuite.apply[(QQRuntimeParams, InputParams)](
    Suite[(QQRuntimeParams, InputParams)]("QQ Runtime Benchmarks")(
      Benchmark.setup[(QQRuntimeParams, InputParams), BenchParams] {
        case (params, inputParams) =>
          val filt: Filter = buildRec(FilterDSL.compose(_, FilterDSL.id), inputParams.size, FilterDSL.id)
          val ready = BenchParams[params.T](QQCompiler.compile(params.runtime, Nil, filt).valueOr(s => sys.error(s.toString)), params.iso(inputParams.input))
          ready
      }.apply("compose with id") {
        params => params.filt(params.in)
      }
    ),
    GuiParams.two(
      Iso[(QQRuntimeParams, InputParams), (QQRuntimeParams, Int)] { case (qrp, ip) => (qrp, ip.size) } { case (qrp, ips) => (qrp, InputParams(size = ips)) },
      GuiParam.enum[QQRuntimeParams]("Runtime", JSRuntimeParams, UpickleRuntimeParams)(
        (_: QQRuntimeParams).toString(),
        initialValues = Seq(JSRuntimeParams, UpickleRuntimeParams)
      ),
      GuiParam.int("Program Size", 1, 5, 10, 20, 50, 100, 200)
    )
  )

  @JSExport
  def main(): Unit = {
    val body = dom.document getElementById "container"
    BenchmarkGUI.renderMenu(body, layout = configureLayout)(qqCompilerSuite, qqRuntimeSuite)
  }

}
