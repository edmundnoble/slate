package qq

import matryoshka._
import qq.FilterDSL._

class LocalOptimizerTest extends QQSyncTestSuite {

  import LocalOptimizer.optimizeFilter

  "optimize simple compositions" in {
    optimizeFilter(compose(id, selectKey("key"))) shouldBe selectKey("key")
    optimizeFilter(compose(id, compose(selectKey("key"), id))) shouldBe selectKey("key")
  }

  "optimize collectresults and enlist duality" in {
    optimizeFilter(collectResults(enlist(id))) shouldBe id
    optimizeFilter(enlist(collectResults(id))) shouldBe id
  }

  "do nested optimizations" in {
    optimizeFilter(
      collectResults(compose(id, compose(id, enlist(id))))
    ) shouldBe id
  }

  "optimize all math in constant expressions" in {
    optimizeFilter(
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

  "optimize constant chains" in {
    optimizeFilter(
      compose(constNumber(20), constNumber(30))
    ) shouldBe constNumber(30)
  }


  "no stack overflow on large filters" taggedAs StackTest in {
    @annotation.tailrec def collectRec(f: ConcreteFilter, i: Int): ConcreteFilter = if (i == 0) f else collectRec(collectResults(f), i - 1)
    @annotation.tailrec def enlistRec(f: ConcreteFilter, i: Int): ConcreteFilter = if (i == 0) f else enlistRec(enlist(f), i - 1)
    optimizeFilter(collectRec(enlistRec(id, 1000), 1000)) shouldBe id
  }

}
