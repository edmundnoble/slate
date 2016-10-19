package dash
package bench

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui.{GuiParam, GuiParams, GuiSuite}
import qq.cc.{CompiledFilter, OrCompilationError, QQCompiler}
import qq.data.{ConcreteFilter, FilterComponent, QQDSL}

object CompilerBench {

  @inline final def runCompilerBench(filt: ConcreteFilter): OrCompilationError[CompiledFilter] =
    QQCompiler.compileFilter(IndexedSeq.empty, filt)

  val qqCompilerSuite: GuiSuite[Int] = GuiSuite(
    Suite("QQ Compiler Benchmarks")(
      Benchmark.setup[Int, ConcreteFilter] { i =>
        Util.buildRec(QQDSL.compose(_, QQDSL.getPathS(QQDSL.selectKey("key"))), i, QQDSL.id)
      }("fix select key")(runCompilerBench),

      Benchmark.setup[Int, ConcreteFilter] { i =>
        Util.buildRec(QQDSL.compose(_, QQDSL.id), i, QQDSL.id)
      }("fix compose with id")(runCompilerBench)
    ), GuiParams.one(GuiParam.int("filter size", 5, 10, 50))
  )

}
