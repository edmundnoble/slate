package qq

import matryoshka._
import qq.FilterDSL._

class OptimizerTest extends QQSyncTestSuite {

  import Optimizer.optimize

  "optimize simple compositions" in {
    optimize(compose(id, selectKey("key"))) shouldBe selectKey("key")
    optimize(compose(id, compose(selectKey("key"), id))) shouldBe selectKey("key")
  }

  "optimize collectresults and enlist duality" in {
    optimize(collectResults(enlist(id))) shouldBe id
    optimize(enlist(collectResults(id))) shouldBe id
  }

  "do nested optimizations" in {
    optimize(
      collectResults(compose(id, compose(id, enlist(id))))
    ) shouldBe id
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
    ) shouldBe constNumber(20 + (5.4 * (1 / (1 - (0.25 * 2)))))
  }

  "no stack overflow on large filters" in {
    def collectRec(f: Filter, i: Int): Filter = if (i == 0) f else collectRec(collectResults(f), i - 1)
    def enlistRec(f: Filter, i: Int): Filter = if (i == 0) f else enlistRec(enlist(f), i - 1)
    optimize(collectRec(enlistRec(id, 1000), 1000)) shouldBe id
  }

}
