package dash
package bench

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui.{GuiParam, GuiParams, GuiSuite}
import qq._
import qq.jsc.JSRuntime

object CompilerBench {

  val qqCompilerSuite = GuiSuite(
    Suite("QQ Compiler Benchmarks")(
      Benchmark.setup[Int, ConcreteFilter] { i =>
        Util.buildRec(FilterDSL.compose(_, FilterDSL.selectKey("key")), i, FilterDSL.id)
      }("select key") { filt =>
        QQCompiler.compile(JSRuntime, IndexedSeq.empty, filt)
      },
      Benchmark.setup[Int, ConcreteFilter] { i =>
        Util.buildRec(FilterDSL.compose(_, FilterDSL.id), i, FilterDSL.id)
      }("compose with id") { filt =>
        QQCompiler.compile(JSRuntime, IndexedSeq.empty, filt)
      }
    ), GuiParams.one(GuiParam.int("filter size", 5, 10, 100, 200))
  )

}
