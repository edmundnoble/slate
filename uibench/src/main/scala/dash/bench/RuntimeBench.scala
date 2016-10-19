package dash
package bench

import dash.bench.BenchmarkApp._
import japgolly.scalajs.benchmark.Benchmark.Builder
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui.{GuiParam, GuiParams, GuiSuite}
import monix.eval.Task
import monocle.Iso
import qq.cc.QQCompiler
import qq.data.{ConcreteFilter, JSON, QQDSL}

object RuntimeBench {

  def runtimeSetup(filt: Int => (ConcreteFilter, String)): Builder[(QQRuntimeParams, Int), BenchParams] =
    Benchmark.setup[(QQRuntimeParams, Int), BenchParams] {
      case (params, size) =>
        val (filter, input) = filt(size)
        val ready = BenchParams(QQCompiler.compileFilter(IndexedSeq.empty, filter).valueOr(s => sys.error(s.toString)), params.iso(input))
        ready
    }

  import japgolly.scalajs.react.vdom.prefix_<^._

  @inline final def runRuntimeBench(params: BenchParams): Task[List[JSON]] =
    params.filt(Map.empty)(params.in).map(_.getOrElse(???))

  val qqRuntimeSuite: GuiSuite[(QQRuntimeParams, Int)] =
    GuiSuite.apply[(QQRuntimeParams, Int)](
      Suite[(QQRuntimeParams, Int)]("QQ Runtime Benchmarks")(
        runtimeSetup(size => (Util.buildRec(QQDSL.compose(_, QQDSL.id), size, QQDSL.id), "[]"))("compose with id")(runRuntimeBench),
        runtimeSetup(size => (QQDSL.getPathS(QQDSL.selectKey(s"k$size")), "{" + Stream.tabulate(size)(i => raw""""k$i":"$i"""").mkString(",") + "}"))("select key")(runRuntimeBench),
        runtimeSetup(size => (Util.buildRec(QQDSL.add(_, QQDSL.constNumber(1)), size, QQDSL.id), s"$size"))("plus")(runRuntimeBench)
      ),
      GuiParams.two(
        Iso.id[(QQRuntimeParams, Int)],
        GuiParam.enum[QQRuntimeParams]("Runtime", QQRuntimeParams)(
          (p: QQRuntimeParams) => <.span(p.toString()),
          initialValues = Seq(QQRuntimeParams)
        ),
        GuiParam.int("Program Size", 1, 5, 10, 20, 50, 100, 200)
      )
    )

}
