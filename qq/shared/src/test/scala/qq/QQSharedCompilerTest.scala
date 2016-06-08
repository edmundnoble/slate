package qq

import qq.QQAST.{ComposeFilters, IdFilter, SelectKey}
import qq.{QQAST, QQParser}
import utest._

object QQSharedCompilerTest extends utest.TestSuite {

  override val tests = TestSuite {

    "optimize simple compositions" - {
      QQAST.optimize(ComposeFilters(IdFilter, SelectKey("key"))).runAttempt.value ==> SelectKey("key")
      QQAST.optimize(ComposeFilters(IdFilter, ComposeFilters(SelectKey("key"), IdFilter))).runAttempt.value ==> SelectKey("key")
    }

    "optimize pipes and dots to the same thing" - {
      QQAST.optimize(QQParser.ensequencedFilters.parse(".key | .dang").get.value)
        .runAttempt.value ==> ComposeFilters(SelectKey("key"), SelectKey("dang"))
      QQAST.optimize(QQParser.ensequencedFilters.parse(".key.dang").get.value)
        .runAttempt.value ==> ComposeFilters(SelectKey("key"), SelectKey("dang"))
    }

  }

}
