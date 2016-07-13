package qq

import scala.concurrent.Future
import TestUtil._
import qq.jsc.JSRuntime
import monix.execution.Scheduler.Implicits.global

class JSCompilerTest extends QQTestSuite {

  import CompilerTest._

  def testRun(qQDoubleCompilerTest: CompilerTest) = qQDoubleCompilerTest match {
    case CompilerTest(filter, input, result) =>
      QQCompiler
        .compile(JSRuntime, Nil, filter)
        .getOrElse(???)(upickle.json.writeJs(input).asInstanceOf[AnyRef])
        .runFuture
        .map(_.map(upickle.json.readJs) should equal(result))
  }

  "select keys" in Future.traverse(selectKeyTest)(testRun)
  "select index" in Future.traverse(selectIndexTest)(testRun)
  "id" in Future.traverse(idTest)(testRun)
  "select range" in Future.traverse(selectRangeTest)(testRun)
  "collect results" in Future.traverse(collectResultsTest)(testRun)

}
