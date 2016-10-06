package qq.macros

import org.scalatest.{FreeSpec, Matchers}
import qq.cc.{LocalOptimizer, Parser}
import qq.data.{ConcreteFilter, Program}
import qq.macros.QQInterpolator._
import qq.util.Recursion
import qq.util.Recursion.RecursionEngine

case class QQInterpolatorTestCase(name: String, programs: List[String], literalResults: List[Program[ConcreteFilter]])

class QQInterpolatorSmokeTest extends FreeSpec with Matchers {
  implicit val recEngine: RecursionEngine =
    Recursion.Unsafe.Direct

  val testCases = List(
    QQInterpolatorTestCase("paths",
      List(".", ". | .", ".lol", ".key.[1].hey"),
      List(qq".", qq". | .", qq".lol", qq".key.[1].hey"))
  )

  val _ =
    testCases.foreach(tc =>
      tc.name in {
        val _ = tc.programs.map(p => LocalOptimizer.optimizeProgram(Parser.program.parse(p).get.value)) shouldBe tc.literalResults
      }
    )
}
