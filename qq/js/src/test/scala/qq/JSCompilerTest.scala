package qq

import org.scalatest.Assertion

import scala.concurrent.Future

class JSCompilerTest extends QQAsyncTestSuite {

  import CompilerTest._

  def runTest(qqCompilerTest: CompilerTest): Future[Assertion] =
    CompilerTest.runTest[Any](qq.jsc.JSRuntime, qqCompilerTest)

  "select keys" in Future.traverse(selectKeyTests)(runTest)
  "select index" in Future.traverse(selectIndexTests)(runTest)
  "id" in Future.traverse(idTests)(runTest)
  "select range" in Future.traverse(selectRangeTests)(runTest)
  "collect results" in Future.traverse(collectResultsTests)(runTest)
  "fat stack" taggedAs StackTest in Future.traverse(fatStackTests)(runTest)

}
