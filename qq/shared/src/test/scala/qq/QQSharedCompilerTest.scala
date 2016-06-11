package qq

import utest._
import matryoshka._
import FunctorT.ops._

object QQSharedCompilerTest extends utest.TestSuite {

  override val tests = TestSuite {

    def optimize(p: QQFilter): QQFilter = p.transCataT(QQOptimizer.optimize)

    "optimize simple compositions" - {
      optimize(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key"))) ==> QQFilter.selectKey("key")
      optimize(QQFilter.compose(QQFilter.id, QQFilter.compose(QQFilter.selectKey("key"), QQFilter.id))) ==> QQFilter.selectKey("key")
    }

    "optimize pipes and dots to the same thing" - {
      val out = QQFilter.compose(QQFilter.selectKey("key"), QQFilter.selectKey("dang"))
      optimize(QQParser.ensequencedFilters.parse(".key | .dang").get.value) ==> out
      optimize(QQParser.ensequencedFilters.parse(".key.dang").get.value) ==> out
    }

  }

}
