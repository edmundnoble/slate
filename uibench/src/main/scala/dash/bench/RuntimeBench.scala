package dash
package bench

import dash.bench.BenchmarkApp.{BenchParams, JSRuntimeParams, QQRuntimeParams, UpickleRuntimeParams}
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui.{GuiParam, GuiParams, GuiSuite}
import matryoshka.Fix
import monocle.Iso
import qq.cc.QQCompiler
import qq.data.{ConcreteFilter, QQDSL}

object RuntimeBench {

  def runtimeSetup(filt: Int => (ConcreteFilter, String)) = Benchmark.setup[(QQRuntimeParams, Int), BenchParams] {
    case (params, size) =>
      val (filter, input) = filt(size)
      val ready = BenchParams[params.T](QQCompiler.compileFilter(params.runtime, IndexedSeq.empty, filter).valueOr(s => sys.error(s.toString)), params.iso(input))
      ready
  }

  import japgolly.scalajs.react.vdom.prefix_<^._

  val qqRuntimeSuite = GuiSuite.apply[(QQRuntimeParams, Int)](
    Suite[(QQRuntimeParams, Int)]("QQ Runtime Benchmarks")(
      runtimeSetup(size => (Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.id), size, QQDSL.fix.id), "[]")
      )("compose with id") {
        params => params.filt(Map.empty)(params.in)
      },
      runtimeSetup(size => (QQDSL.fix.getPathS(QQDSL.fix.selectKey(s"k$size")), "{" + Stream.tabulate(size)(i => raw""""k$i":"$i"""").mkString(",") + "}")
      )("select key") {
        params => params.filt(Map.empty)(params.in)
      },
      runtimeSetup(size => (Util.buildRec[Fix](QQDSL.fix.add(_, QQDSL.fix.constNumber(1)), size, QQDSL.fix.id), s"$size")
      )("plus") {
        params => params.filt(Map.empty)(params.in)
      }
    ),
    GuiParams.two(
      Iso.id[(QQRuntimeParams, Int)],
      GuiParam.enum[QQRuntimeParams]("Runtime", JSRuntimeParams, UpickleRuntimeParams)(
        (p: QQRuntimeParams) => <.span(p.toString()),
        initialValues = Seq(JSRuntimeParams, UpickleRuntimeParams)
      ),
      GuiParam.int("Program Size", 1, 5, 10, 20, 50, 100, 200)
    )
  )

}
