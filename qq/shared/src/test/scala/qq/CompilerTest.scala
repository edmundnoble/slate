package qq

import monix.eval.Task
import org.scalatest.Assertion
import upickle.Js
import monix.execution.Scheduler
import AsyncTestUtil._

import scala.concurrent.Future

case class CompilerTest(program: Filter, input: Js.Value, expectedOutput: List[Js.Value])

object CompilerTest {

  import org.scalatest.Matchers._
  import FilterDSL._

  def runTest[AnyTy](runtime: QQRuntime[AnyTy], qqCompilerTest: CompilerTest)
                    (implicit sch: Scheduler): Future[Assertion] = qqCompilerTest match {
    case CompilerTest(filter, input, expectedOutput) =>
      QQCompiler
        .compile(UpickleRuntime, Nil, filter)
        .fold[Task[Assertion]](
        err => Task.evalAlways(fail(s"error occurred during compilation: $err")),
        program => program(input).map { output => output shouldBe expectedOutput })
        .runFuture
  }

  val selectKeyTests: List[CompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(
      CompilerTest(selectKey("present"), dict, List(Js.Num(1))),
      CompilerTest(selectKey("absent"), dict, List(Js.Null))
    )
  }

  val selectIndexTests: List[CompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2))
    List(
      CompilerTest(selectIndex(-3), arr, List(Js.Null)),
      CompilerTest(selectIndex(-2), arr, List(Js.Num(1))),
      CompilerTest(selectIndex(-1), arr, List(Js.Num(2))),
      CompilerTest(selectIndex(0), arr, List(Js.Num(1))),
      CompilerTest(selectIndex(1), arr, List(Js.Num(2))),
      CompilerTest(selectIndex(2), arr, List(Js.Null))
    )
  }

  val idTests: List[CompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(CompilerTest(id, dict, List(dict)))
  }

  val selectRangeTests: List[CompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))
    List(
      CompilerTest(selectRange(0, 0), arr, List(Js.Arr())),
      CompilerTest(selectRange(0, 1), arr, List(Js.Arr(Js.Num(1)))),
      CompilerTest(selectRange(0, 2), arr, List(Js.Arr(Js.Num(1), Js.Num(2)))),
      CompilerTest(selectRange(0, 3), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3)))),
      CompilerTest(selectRange(0, 4), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      CompilerTest(selectRange(0, 5), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      CompilerTest(selectRange(1, 5), arr, List(Js.Arr(Js.Num(2), Js.Num(3), Js.Num(4))))
    )
  }

  val collectResultsTests: List[CompilerTest] = {
    List(
      CompilerTest(collectResults(id),
        Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)),
        List(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))),
      CompilerTest(collectResults(id),
        Js.Obj("a" -> Js.Num(1), "b" -> Js.Str("c")),
        List(Js.Num(1), Js.Str("c")))
    )
  }

  val fatStackTests: List[CompilerTest] = {
    def fun(f: Filter, i: Int): Filter = if (i == 0) f else fun(compose(id, f), i - 1)
    List(
      CompilerTest(
        fun(id, 1000), Js.False, List(Js.False)
      )
    )
  }

}
