package qq
package macros

import org.scalatest.{FreeSpec, Matchers}
import qq.ast.LocalOptimizer
import qq.cc.Parser
import qq.data.{FilterAST, Program}
import qq.macros.stager.QQStager._
import qq.util.Recursion
import qq.util.Recursion.RecursionEngine

case class QQStagerTestCase(name: String, programs: Vector[String], literalResults: Vector[Program[FilterAST]])

class QQStagerSmokeTest extends FreeSpec with Matchers {
  implicit val recEngine: RecursionEngine =
    Recursion.Unsafe.Direct

  val testCases = Vector(
    QQStagerTestCase("paths",
      Vector(".", ". | .", ".lol", ".key.[1].hey"),
      Vector(qq".", qq". | .", qq".lol", qq".key.[1].hey"))
  )

  val _ =
    testCases.foreach(tc =>
      tc.name in {
        val _ = tc.programs.map(p => LocalOptimizer.optimizeProgram(Parser.program.parse(p).get.value)) shouldBe tc.literalResults
      }
    )
}
