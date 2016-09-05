package qq

import monix.eval.Task
import org.scalatest.Assertion
import upickle.Js
import monix.execution.Scheduler
import upickle.Js.Value

import scala.concurrent.Future

case class CompilerTest(input: Value, program: Filter, expectedOutput: Value*)

object CompilerTest {

  import org.scalatest.Matchers._
  import FilterDSL._

  def runTest[AnyTy](runtime: QQRuntime[AnyTy], qqCompilerTest: CompilerTest)
                    (implicit sch: Scheduler): Future[Assertion] = qqCompilerTest match {
    case CompilerTest(input, filter, expectedOutput@_*) =>
      QQCompiler
        .compile(UpickleRuntime, IndexedSeq.empty, filter)
        .fold[Task[Assertion]](
        err => Task.eval(fail("error occurred during compilation: \n" + err.toString)),
        program => program(input).map { output => output shouldBe expectedOutput.toList })
        .runAsync
  }

  val selectKeyTests: List[CompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(
      CompilerTest(dict, selectKey("present"), Js.Num(1)),
      CompilerTest(dict, selectKey("absent"), Js.Null)
    )
  }

  val selectIndexTests: List[CompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2))
    List(
      CompilerTest(arr, selectIndex(-3), Js.Null),
      CompilerTest(arr, selectIndex(-2), Js.Num(1)),
      CompilerTest(arr, selectIndex(-1), Js.Num(2)),
      CompilerTest(arr, selectIndex(0), Js.Num(1)),
      CompilerTest(arr, selectIndex(1), Js.Num(2)),
      CompilerTest(arr, selectIndex(2), Js.Null)
    )
  }

  val idTests: List[CompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(CompilerTest(dict, id, dict))
  }

  val selectRangeTests: List[CompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))
    List(
      CompilerTest(arr, selectRange(0, 0), Js.Arr()),
      CompilerTest(arr, selectRange(0, 1), Js.Arr(Js.Num(1))),
      CompilerTest(arr, selectRange(0, 2), Js.Arr(Js.Num(1), Js.Num(2))),
      CompilerTest(arr, selectRange(0, 3), Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3))),
      CompilerTest(arr, selectRange(0, 4), Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))),
      CompilerTest(arr, selectRange(0, 5), Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))),
      CompilerTest(arr, selectRange(1, 5), Js.Arr(Js.Num(2), Js.Num(3), Js.Num(4)))
    )
  }

  val collectResultsTests: List[CompilerTest] = {
    List(
      CompilerTest(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)), collectResults(id), Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)),
      CompilerTest(Js.Obj("a" -> Js.Num(1), "b" -> Js.Str("c")), collectResults(id), Js.Num(1), Js.Str("c"))
    )
  }

  val fatStackTests: List[CompilerTest] = {
    // tail recursive builder, giving you (i * 2) compositions of id with f.
    @annotation.tailrec
    def fun(f: Filter, i: Int): Filter = if (i == 0) f else fun(compose(compose(id, f), id), i - 1)
    List(
      CompilerTest(Js.False, fun(id, 1000), Js.False)
    )
  }

}
