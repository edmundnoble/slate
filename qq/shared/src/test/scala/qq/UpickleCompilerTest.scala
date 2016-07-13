package qq

import scala.concurrent.Future
import TestUtil._

import monix.execution.Scheduler.Implicits.global

class UpickleCompilerTest extends QQTestSuite {

  import CompilerTest._

  def testRun(qQDoubleCompilerTest: CompilerTest) = qQDoubleCompilerTest match {
    case CompilerTest(filter, input, result) =>
      QQCompiler
        .compile(UpickleRuntime, Nil, filter)
        .getOrElse(???)(input)
        .runFuture
        .map (_ should equal(result))
  }

  "select keys" in Future.traverse(selectKeyTest)(testRun)
  "select index" in Future.traverse(selectIndexTest)(testRun)
  "id" in Future.traverse(idTest)(testRun)
  "select range" in Future.traverse(selectRangeTest)(testRun)
  "collect results" in Future.traverse(collectResultsTest)(testRun)

}
