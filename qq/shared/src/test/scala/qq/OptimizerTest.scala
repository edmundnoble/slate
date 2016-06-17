package qq

import utest._
import matryoshka._

object OptimizerTest extends utest.TestSuite with Asserts {

  override val tests = TestSuite {

    "optimize simple compositions" - {
      Optimizer.optimize(Filter.compose(Filter.id, Filter.selectKey("key"))) ===>
        Filter.selectKey("key")
      Optimizer.optimize(Filter.compose(Filter.id, Filter.compose(Filter.selectKey("key"), Filter.id))) ===>
        Filter.selectKey("key")
    }

  }

}
