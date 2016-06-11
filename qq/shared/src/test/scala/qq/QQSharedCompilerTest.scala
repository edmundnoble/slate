package qq

import qq.QQAST.{ComposeFilters, IdFilter, QQProgram, SelectKey}
import qq.{QQAST, QQParser}
import utest._
import matryoshka._, FunctorT.ops._

object QQSharedCompilerTest extends utest.TestSuite {

  override val tests = TestSuite {

    def optimize(p: QQProgram): QQProgram = p.transCataT(QQAST.optimize)

    import QQAST.QQFilter

    "optimize simple compositions" - {
      optimize(QQProgram.compose(QQProgram.id, QQProgram.selectKey("key"))) ==> QQProgram.selectKey("key")
      optimize(QQProgram.compose(QQProgram.id, QQProgram.compose(QQProgram.selectKey("key"), QQProgram.id))) ==> QQProgram.selectKey("key")
    }

    "optimize pipes and dots to the same thing" - {
      val out = QQProgram.compose(QQProgram.selectKey("key"), QQProgram.selectKey("dang"))
      optimize(QQParser.ensequencedFilters.parse(".key | .dang").get.value) ==> out
      optimize(QQParser.ensequencedFilters.parse(".key.dang").get.value) ==> out
    }

  }

}
