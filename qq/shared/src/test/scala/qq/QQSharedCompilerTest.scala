package qq

import utest._
import matryoshka._

object QQSharedCompilerTest extends utest.TestSuite {

  override val tests = TestSuite {

    "optimize simple compositions" - {
      QQOptimizer.optimize(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key"))) ==>
        QQFilter.selectKey("key")
      QQOptimizer.optimize(QQFilter.compose(QQFilter.id, QQFilter.compose(QQFilter.selectKey("key"), QQFilter.id))) ==>
        QQFilter.selectKey("key")
    }

    "optimize pipes and dots to the same thing" - {
      val out = QQFilter.compose(QQFilter.selectKey("key"), QQFilter.selectKey("dang"))
      QQOptimizer.optimize(QQParser.ensequencedFilters.parse(".key | .dang").get.value) ==> out
      QQOptimizer.optimize(QQParser.ensequencedFilters.parse(".key.dang").get.value) ==> out
    }

    "optimizations should compose properly" - {
      QQOptimizer.optimize(QQFilter.compose(QQFilter.ensequence(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")) :: Nil), QQFilter.selectKey("dang"))) ==>
        QQFilter.compose(QQFilter.selectKey("key"), QQFilter.selectKey("dang"))
      QQOptimizer.optimize(QQFilter.ensequence(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")) :: Nil)) ==>
        QQFilter.selectKey("key")
    }

  }

}
