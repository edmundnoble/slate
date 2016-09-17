package dash
package bench

import dash.bench.BenchmarkApp.{BenchParams, JSRuntimeParams, QQRuntimeParams, UpickleRuntimeParams}
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui.{GuiParam, GuiParams, GuiSuite}
import matryoshka.Fix
import monocle.Iso
import qq._
import qq.data.{ConcreteFilter, FilterDSL}

object RuntimeBench {

  def runtimeSetup(filt: Int => (ConcreteFilter, String)) = Benchmark.setup[(QQRuntimeParams, Int), BenchParams] {
    case (params, size) =>
      val (filter, input) = filt(size)
      val ready = BenchParams[params.T](QQCompiler.compile(params.runtime, IndexedSeq.empty, filter).valueOr(s => sys.error(s.toString)), params.iso(input))
      ready
  }

  import japgolly.scalajs.react.vdom.prefix_<^._

  val qqRuntimeSuite = GuiSuite.apply[(QQRuntimeParams, Int)](
    Suite[(QQRuntimeParams, Int)]("QQ Runtime Benchmarks")(
      runtimeSetup(size => (Util.buildRec[Fix](FilterDSL.fix.compose(_, FilterDSL.fix.id), size, FilterDSL.fix.id), "[]")
      )("compose with id") {
        params => params.filt(Map.empty)(params.in)
      },
      runtimeSetup(size => (FilterDSL.fix.selectKey(s"k$size"), "{" + Stream.tabulate(size)(i => raw""""k$i":"$i"""").mkString(",") + "}")
      )("select key") {
        params => params.filt(Map.empty)(params.in)
      },
      runtimeSetup(size => (Util.buildRec[Fix](FilterDSL.fix.add(_, FilterDSL.fix.constNumber(1)), size, FilterDSL.fix.id), s"$size")
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
