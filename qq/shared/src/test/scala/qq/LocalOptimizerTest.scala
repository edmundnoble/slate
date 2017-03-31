package qq

import qq.data.FilterAST
import qq.data.QQDSL._
import qq.util.Recursion.RecursionEngine

class LocalOptimizerTest extends QQSyncTestSuite {

  import qq.ast.LocalOptimizer.optimizeFilter

  "optimize simple compositions" in {
    optimizeFilter(id | selectKey("key")) shouldBe getPathS(selectKey("key"))
    optimizeFilter(id | selectKey("key") | id) shouldBe getPathS(selectKey("key"))
  }

  "optimize collectresults and enlist duality" in {
    optimizeFilter(enlist(collectResults)) shouldBe id
    optimizeFilter(enlist(compose(collectResults, id))) shouldBe id
    optimizeFilter(enlist(id | collectResults)) shouldBe id
  }

  "do nested optimizations" in {
    optimizeFilter(
      enlist(id | collectResults | id | id | id)
    ) shouldBe id
  }

  "optimize all math in constant expressions" in {
    optimizeFilter(
      constNumber(5.4) *
        constNumber(1) /
        (constNumber(1) - constNumber(0.25) * constNumber(2)) +
        constNumber(20)
    ) shouldBe constNumber(20 + 5.4 * (1 / (1 - 0.25 * 2)))
  }

  "unlet" in {
    optimizeFilter(
      asBinding("foo", constNumber(1), deref("foo"))
    ) shouldBe constNumber(1)
  }

  "letFree" in {
    optimizeFilter(
      asBinding("foo", constNumber(1), getPath(Vector(selectKey("key1"), selectKey("key2"))) | id + id)
    ) shouldBe getPath(Vector(selectKey("key1"), selectKey("key2"))) | id + id
  }

  "fuseGetPathOperation" in {
    optimizeFilter(
      getPath(Vector(selectKey("key1"), selectKey("key2"))) | getPath(Vector(selectKey("key3"), selectKey("key4")))
    ) shouldBe getPath(Vector(selectKey("key1"), selectKey("key2"), selectKey("key3"), selectKey("key4")))
  }

  "no stack overflow on deeply nested filters" taggedAs StackTest in {
    @annotation.tailrec def enlistRec(f: FilterAST, i: Int): FilterAST =
      if (i == 0) f
      else enlistRec(enlist(f), i - 1)

    val platformRecEngine: RecursionEngine = qq.Platform.Rec.defaultRecScheme

    optimizeFilter(enlistRec(collectResults, 1000))(platformRecEngine) shouldBe enlistRec(id, 999)
  }

}
