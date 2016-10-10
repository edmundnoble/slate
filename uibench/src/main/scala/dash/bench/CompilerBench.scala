package dash
package bench

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui.{GuiParam, GuiParams, GuiSuite}
import matryoshka.Recursive.ops._
import matryoshka.{Corecursive, Fix, Mu, Nu, Recursive}
import qq.cc.{CompiledFilter, OrCompilationError, QQCompiler}
import qq.data.{ConcreteFilter, FilterComponent, QQDSL}

object CompilerBench {

  @inline final def runCompilerBench[T[_[_]] : Recursive : Corecursive](filt: T[FilterComponent]): OrCompilationError[CompiledFilter] =
    QQCompiler.compileFilter(IndexedSeq.empty, filt)

  val qqCompilerSuite: GuiSuite[Int] = GuiSuite(
    Suite("QQ Compiler Benchmarks")(
      Benchmark.setup[Int, ConcreteFilter] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.getPathS(QQDSL.fix.selectKey("key"))), i, QQDSL.fix.id)
      }("fix select key")(runCompilerBench[Fix]),

      Benchmark.setup[Int, ConcreteFilter] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.id), i, QQDSL.fix.id)
      }("fix compose with id")(runCompilerBench[Fix]),

      Benchmark.setup[Int, Mu[FilterComponent]] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.getPathS(QQDSL.fix.selectKey("key"))), i, QQDSL.fix.id).convertTo[Mu]
      }("mu select key")(runCompilerBench[Mu]),

      Benchmark.setup[Int, Mu[FilterComponent]] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.id), i, QQDSL.fix.id).convertTo[Mu]
      }("mu compose with id")(runCompilerBench[Mu]),

      Benchmark.setup[Int, Nu[FilterComponent]] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.getPathS(QQDSL.fix.selectKey("key"))), i, QQDSL.fix.id).convertTo[Nu]
      }("nu select key")(runCompilerBench[Nu]),

      Benchmark.setup[Int, Nu[FilterComponent]] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.id), i, QQDSL.fix.id).convertTo[Nu]
      }("nu compose with id")(runCompilerBench[Nu])
    ), GuiParams.one(GuiParam.int("filter size", 5, 10, 50))
  )

}
