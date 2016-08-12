package qq

import org.scalatest.Assertion

import scala.concurrent.Future

class UpickleCompilerTest extends QQAsyncTestSuite {

  import CompilerTest._

  def runTest(qQDoubleCompilerTest: CompilerTest): Future[Assertion] =
    CompilerTest.runTest(UpickleRuntime, qQDoubleCompilerTest)

  "select keys" in Future.traverse(selectKeyTests)(runTest)
  "select index" in Future.traverse(selectIndexTests)(runTest)
  "id" in Future.traverse(idTests)(runTest)
  "select range" in Future.traverse(selectRangeTests)(runTest)
  "collect results" in Future.traverse(collectResultsTests)(runTest)
  "fat stack" in Future.traverse(fatStackTests)(runTest)

}
