package qq

import utest._
import matryoshka._
import qq.Filter._

object OptimizerTest extends utest.TestSuite with Asserts {

  override val tests = TestSuite {

    "optimize simple compositions" - {
      Optimizer.optimize(compose(id, selectKey("key"))) ===>
        selectKey("key")
      Optimizer.optimize(compose(id, compose(selectKey("key"), id))) ===>
        selectKey("key")
    }

    "optimize collectresults and enlist duality" - {
      Optimizer.optimize(collectResults(enlist(id))) ===> id
      Optimizer.optimize(enlist(collectResults(id))) ===> id
    }

    "do nested optimizations" - {
      Optimizer.optimize(
        collectResults(compose(id, compose(id, enlist(id))))
      ) ===>
        id
    }

    "optimize all math in constant expressions" - {
      Optimizer.optimize(
        add(
          multiply(
            constNumber(5.4),
            divide(
              constNumber(1),
              subtract(
                constNumber(1),
                multiply(
                  constNumber(0.25),
                  constNumber(2)
                )
              )
            )
          ),
          constNumber(20))
      ) ===>
        constNumber(20 + (5.4 * (1 / (1 - (0.25 * 2)))))
    }

  }

}
