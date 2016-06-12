package qq

import utest._
import matryoshka._

object QQOptimizerTest extends utest.TestSuite {

  override val tests = TestSuite {

    "optimize simple compositions" - {
      QQOptimizer.optimize(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key"))) ==>
        QQFilter.selectKey("key")
      QQOptimizer.optimize(QQFilter.compose(QQFilter.id, QQFilter.compose(QQFilter.selectKey("key"), QQFilter.id))) ==>
        QQFilter.selectKey("key")
    }

  }

}
