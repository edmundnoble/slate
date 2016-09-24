package qq

import qq.data.QQDSL.fix._
import qq.data.{ConcreteFilter, FilterComponent}
import qq.util.Recursion

class LocalOptimizerTest extends QQSyncTestSuite {

  import qq.cc.LocalOptimizer.optimizeFilter

  "optimize simple compositions" in {
    optimizeFilter(id | selectKey("key")) shouldBe getPathS(selectKey("key"))
    optimizeFilter(id | selectKey("key") | id) shouldBe getPathS(selectKey("key"))
  }

  "optimize collectresults and enlist duality" in {
    optimizeFilter(enlist(collectResults)) shouldBe id
    optimizeFilter(enlist(compose(collectResults, id)) )shouldBe id
    optimizeFilter(enlist(id | collectResults)) shouldBe id
  }

  "do nested optimizations" in {
    optimizeFilter(
      enlist(id | collectResults | id | id | id)
    ) shouldBe id
  }

  "optimize all math in constant expressions" in {
    optimizeFilter(
      add(
        constNumber(5.4) *
          divide(
            constNumber(1),
            subtract(
              constNumber(1),
              multiply(
                constNumber(0.25),
                constNumber(2)
              )
            )
          ),
        constNumber(20))
    ) shouldBe constNumber(20 + (5.4 * (1 / (1 - (0.25 * 2)))))
  }

  "no stack overflow on deeply nested filters" taggedAs StackTest in {
    @annotation.tailrec def enlistRec(f: ConcreteFilter, i: Int): ConcreteFilter = if (i == 0) f else enlistRec(enlist(f), i - 1)

    optimizeFilter(enlistRec(collectResults, 1000)) shouldBe enlistRec(id, 999)
  }

}
