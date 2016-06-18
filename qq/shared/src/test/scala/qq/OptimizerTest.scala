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

    "optimize collectresults and enlist duality" - {
      Optimizer.optimize(Filter.collectResults(Filter.enlist(Filter.id))) ===> Filter.id
      Optimizer.optimize(Filter.enlist(Filter.collectResults(Filter.id))) ===> Filter.id
    }

    "do nested optimizations" - {
      Optimizer.optimize(
        Filter.collectResults(
          Filter.compose(
            Filter.id,
            Filter.compose(
              Filter.id,
              Filter.enlist(Filter.id)
            )
          )
        )
      ) ===>
        Filter.id
    }

    "optimize complicated math" - {
      Optimizer.optimize(
        Filter.add(
          Filter.multiply(
            Filter.constNumber(5.4),
            Filter.divide(
              Filter.constNumber(1),
              Filter.constNumber(0.5)
            )
          ),
          Filter.constNumber(20))
      ) ===>
        Filter.constNumber(20 + (5.4 * (1 / 0.5)))
    }

  }

}
