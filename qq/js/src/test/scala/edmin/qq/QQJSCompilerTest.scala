package edmin.qq

import utest._

import scala.concurrent.Future
import Util._

import monix.execution.Scheduler.Implicits.global

object QQJSCompilerTest extends utest.TestSuite {

  val tests = this {

    import edmin.qq.jsc.QQJSCompiler._

    def testRun(qQDoubleCompilerTest: QQDoubleCompilerTest) = qQDoubleCompilerTest match {
      case QQDoubleCompilerTest(filter, input, result) =>
        compile(Nil, filter).getOrElse(???).runAttempt.value.apply(upickle.json.writeJs(input).asInstanceOf[scalajs.js.Any]).runFuture map (_.map(upickle.json.readJs) ==> result)
    }

    "select keys" - Future.traverse(QQDoubleCompilerTest.selectKeys)(testRun)
    "select index" - Future.traverse(QQDoubleCompilerTest.selectIndex)(testRun)
    "id" - Future.traverse(QQDoubleCompilerTest.id)(testRun)
    "select range" - Future.traverse(QQDoubleCompilerTest.selectRange)(testRun)
    "collect results" - Future.traverse(QQDoubleCompilerTest.collectResults)(testRun)

  }
}
