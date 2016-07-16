package qq

import upickle.Js

case class CompilerTest(program: Filter, input: Js.Value, expectedOutput: List[Js.Value])

object CompilerTest {

  import FilterDSL._

  val selectKeyTest: List[CompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(
      CompilerTest(selectKey("present"), dict, List(Js.Num(1))),
      CompilerTest(selectKey("absent"), dict, List(Js.Null))
    )
  }

  val selectIndexTest: List[CompilerTest] = {
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

  val idTest: List[CompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(CompilerTest(id, dict, List(dict)))
  }

  val selectRangeTest: List[CompilerTest] = {
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

  val collectResultsTest: List[CompilerTest] = {
    List(
      CompilerTest(collectResults(id),
        Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)),
        List(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))),
      CompilerTest(collectResults(id),
        Js.Obj("a" -> Js.Num(1), "b" -> Js.Str("c")),
        List(Js.Num(1), Js.Str("c")))
    )
  }

  val fatStackTest = {
    def fun(f: Filter, i: Int): Filter = if (i == 0) f else fun(compose(id, f), i - 1)
    CompilerTest(
      fun(id, 1000), Js.False, List(Js.False)
    )
  }

}
