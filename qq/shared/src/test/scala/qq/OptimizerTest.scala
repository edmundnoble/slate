package qq

import matryoshka._
import qq.FilterDSL._

class OptimizerTest extends QQTestSuite {

  import Optimizer.optimize

  "optimize simple compositions" in {
    optimize(compose(id, selectKey("key"))) should equal(selectKey("key"))
    optimize(compose(id, compose(selectKey("key"), id))) should equal(selectKey("key"))
  }

  "optimize collectresults and enlist duality" in {
    optimize(collectResults(enlist(id))) should equal(id)
    optimize(enlist(collectResults(id))) should equal(id)
  }

  "do nested optimizations" in {
    optimize(
      collectResults(compose(id, compose(id, enlist(id))))
    ) should equal(id)
  }

  "optimize all math in constant expressions" in {
    optimize(
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
    ) should equal(constNumber(20 + (5.4 * (1 / (1 - (0.25 * 2))))))
  }

}
