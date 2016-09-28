package dash
package bench

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui.{GuiParam, GuiParams, GuiSuite}
import matryoshka.{Fix, Mu, Nu}
import matryoshka.Recursive.ops._
import qq.cc.QQCompiler
import qq.data.{ConcreteFilter, FilterComponent, QQDSL}
import qq.cc.jsc.JSRuntime

object CompilerBench {

  val qqCompilerSuite: GuiSuite[Int] = GuiSuite(
    Suite("QQ Compiler Benchmarks")(
      Benchmark.setup[Int, ConcreteFilter] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.getPathS(QQDSL.fix.selectKey("key"))), i, QQDSL.fix.id)
      }("fix select key") { filt =>
        QQCompiler.compileFilter(JSRuntime, IndexedSeq.empty, filt)
      },

      Benchmark.setup[Int, ConcreteFilter] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.id), i, QQDSL.fix.id)
      }("fix compose with id") { filt =>
        QQCompiler.compileFilter(JSRuntime, IndexedSeq.empty, filt)
      },

      Benchmark.setup[Int, Mu[FilterComponent]] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.getPathS(QQDSL.fix.selectKey("key"))), i, QQDSL.fix.id).convertTo[Mu]
      }("mu select key") { filt =>
        QQCompiler.compileFilter(JSRuntime, IndexedSeq.empty, filt)
      },

      Benchmark.setup[Int, Mu[FilterComponent]] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.id), i, QQDSL.fix.id).convertTo[Mu]
      }("mu compose with id") { filt =>
        QQCompiler.compileFilter(JSRuntime, IndexedSeq.empty, filt)
      },

      Benchmark.setup[Int, Nu[FilterComponent]] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.getPathS(QQDSL.fix.selectKey("key"))), i, QQDSL.fix.id).convertTo[Nu]
      }("nu select key") { filt =>
        QQCompiler.compileFilter(JSRuntime, IndexedSeq.empty, filt)
      },

      Benchmark.setup[Int, Nu[FilterComponent]] { i =>
        Util.buildRec[Fix](QQDSL.fix.compose(_, QQDSL.fix.id), i, QQDSL.fix.id).convertTo[Nu]
      }("nu compose with id") { filt =>
        QQCompiler.compileFilter(JSRuntime, IndexedSeq.empty, filt)
      }
    ), GuiParams.one(GuiParam.int("filter size", 5, 10, 50))
  )

}
