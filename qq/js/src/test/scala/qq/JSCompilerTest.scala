package qq

import scala.concurrent.Future
import TestUtil._
import monix.execution.Scheduler.Implicits.global
import qq.jsc.JSRuntime

class JSCompilerTest extends QQTestSuite {

  def testRun(qQDoubleCompilerTest: CompilerTest) = qQDoubleCompilerTest match {
    case CompilerTest(filter, input, result) =>
      QQCompiler
        .compile(JSRuntime, Nil, filter)
        .getOrElse(???)(upickle.json.writeJs(input).asInstanceOf[AnyRef])
        .runFuture
        .map(_.map(upickle.json.readJs) should equal(result))
  }

  "select keys" in Future.traverse(CompilerTest.selectKeys)(testRun)
  "select index" in Future.traverse(CompilerTest.selectIndex)(testRun)
  "id" in Future.traverse(CompilerTest.id)(testRun)
  "select range" in Future.traverse(CompilerTest.selectRange)(testRun)
  "collect results" in Future.traverse(CompilerTest.collectResults)(testRun)

}
