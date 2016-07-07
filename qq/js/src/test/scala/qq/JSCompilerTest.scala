package qq

import utest._

import scala.concurrent.Future
import TestUtil._
import monix.execution.Scheduler.Implicits.global
import qq.jsc.JSRuntime

object JSCompilerTest extends utest.TestSuite with Asserts {

  val tests = this {

    def testRun(qQDoubleCompilerTest: CompilerTest) = qQDoubleCompilerTest match {
      case CompilerTest(filter, input, result) =>
        QQCompiler
          .compile(JSRuntime, Nil, filter)
          .getOrElse(???)(upickle.json.writeJs(input).asInstanceOf[AnyRef])
          .runFuture
          .map(_.map(upickle.json.readJs) ===> result)
    }

    "select keys" - Future.traverse(CompilerTest.selectKeys)(testRun)
    "select index" - Future.traverse(CompilerTest.selectIndex)(testRun)
    "id" - Future.traverse(CompilerTest.id)(testRun)
    "select range" - Future.traverse(CompilerTest.selectRange)(testRun)
    "collect results" - Future.traverse(CompilerTest.collectResults)(testRun)

  }
}
