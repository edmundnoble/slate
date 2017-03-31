package slate
package bench

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui.{GuiParam, GuiParams, GuiSuite}
import qq.Platform.Rec._
import qq.cc.{CompiledFilter, QQCompiler}
import qq.data.{FilterAST, QQDSL}

object CompilerBench {

  @inline final def runCompilerBench(filt: FilterAST): OrCompileError[CompiledFilter] =
    QQCompiler.compileFilter(Vector.empty, filt)

  val qqCompilerSuite: GuiSuite[Int] = GuiSuite(
    Suite("QQ Compiler Benchmarks")(
      Benchmark.setup[Int, FilterAST] { i =>
        Util.buildRec(QQDSL.compose(_, QQDSL.getPathS(QQDSL.selectKey("key"))), i, QQDSL.id)
      }("fix select key")(runCompilerBench),

      Benchmark.setup[Int, FilterAST] { i =>
        Util.buildRec(QQDSL.compose(_, QQDSL.id), i, QQDSL.id)
      }("fix compose with id")(runCompilerBench)
    ), GuiParams.one(GuiParam.int("filter size", 5, 10, 50))
  )

}
