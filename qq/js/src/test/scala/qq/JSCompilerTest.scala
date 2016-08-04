package qq

import org.scalatest.Assertion
import TestUtil._

import scala.concurrent.Future

class JSCompilerTest extends QQTestSuite {

  import CompilerTest._

  def runTest(qQDoubleCompilerTest: CompilerTest): Future[Assertion] = CompilerTest.runTest(qq.jsc.JSRuntime, qQDoubleCompilerTest)

  "select keys" in Future.traverse(selectKeyTests)(runTest)
  "select index" in Future.traverse(selectIndexTests)(runTest)
  "id" in Future.traverse(idTests)(runTest)
  "select range" in Future.traverse(selectRangeTests)(runTest)
  "collect results" in Future.traverse(collectResultsTests)(runTest)
  "fat stack" in Future.traverse(fatStackTests)(runTest)

}
