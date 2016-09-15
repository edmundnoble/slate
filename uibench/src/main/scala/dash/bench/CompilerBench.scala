package dash
package bench

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui.{GuiParam, GuiParams, GuiSuite}
import matryoshka.{Fix, Mu, Nu}
import matryoshka.Recursive.ops._
import qq._
import qq.jsc.JSRuntime

object CompilerBench {

  val qqCompilerSuite: GuiSuite[Int] = GuiSuite(
    Suite("QQ Compiler Benchmarks")(
      Benchmark.setup[Int, ConcreteFilter] { i =>
        Util.buildRec[Fix](FilterDSL.fix.compose(_, FilterDSL.fix.selectKey("key")), i, FilterDSL.fix.id)
      }("fix select key") { filt =>
        QQCompiler.compile(JSRuntime, IndexedSeq.empty, filt)
      },
      Benchmark.setup[Int, ConcreteFilter] { i =>
        Util.buildRec[Fix](FilterDSL.fix.compose(_, FilterDSL.fix.id), i, FilterDSL.fix.id)
      }("fix compose with id") { filt =>
        QQCompiler.compile(JSRuntime, IndexedSeq.empty, filt)
      },
      Benchmark.setup[Int, Mu[FilterComponent]] { i =>
        Util.buildRec[Fix](FilterDSL.fix.compose(_, FilterDSL.fix.selectKey("key")), i, FilterDSL.fix.id).convertTo[Mu]
      }("mu select key") { filt =>
        QQCompiler.compile(JSRuntime, IndexedSeq.empty, filt)
      },
      Benchmark.setup[Int, Mu[FilterComponent]] { i =>
        Util.buildRec[Fix](FilterDSL.fix.compose(_, FilterDSL.fix.id), i, FilterDSL.fix.id).convertTo[Mu]
      }("mu compose with id") { filt =>
        QQCompiler.compile(JSRuntime, IndexedSeq.empty, filt)
      },
      Benchmark.setup[Int, Nu[FilterComponent]] { i =>
        Util.buildRec[Fix](FilterDSL.fix.compose(_, FilterDSL.fix.selectKey("key")), i, FilterDSL.fix.id).convertTo[Nu]
      }("nu select key") { filt =>
        QQCompiler.compile(JSRuntime, IndexedSeq.empty, filt)
      },
      Benchmark.setup[Int, Nu[FilterComponent]] { i =>
        Util.buildRec[Fix](FilterDSL.fix.compose(_, FilterDSL.fix.id), i, FilterDSL.fix.id).convertTo[Nu]
      }("nu compose with id") { filt =>
        QQCompiler.compile(JSRuntime, IndexedSeq.empty, filt)
      }
    ), GuiParams.one(GuiParam.int("filter size", 5, 10, 50))
  )

}
