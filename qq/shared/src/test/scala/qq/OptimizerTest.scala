package qq

import matryoshka._
import qq.Filter._

class OptimizerTest extends QQTestSuite {

  "optimize simple compositions" in {
    Optimizer.optimize(compose(id, selectKey("key"))) should equal(selectKey("key"))
    Optimizer.optimize(compose(id, compose(selectKey("key"), id))) should equal(selectKey("key"))
  }

  "optimize collectresults and enlist duality" in {
    Optimizer.optimize(collectResults(enlist(id))) should equal(id)
    Optimizer.optimize(enlist(collectResults(id))) should equal(id)
  }

  "do nested optimizations" in {
    Optimizer.optimize(
      collectResults(compose(id, compose(id, enlist(id))))
    ) should equal(id)
  }

  "optimize all math in constant expressions" in {
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
    ) should equal(constNumber(20 + (5.4 * (1 / (1 - (0.25 * 2))))))
  }

}
