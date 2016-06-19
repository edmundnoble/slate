package qq

import upickle.Js

case class CompilerTest(program: Filter, input: Js.Value, expectedOutput: List[Js.Value])

object CompilerTest {

  val selectKeys: List[CompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(
      CompilerTest(Filter.selectKey("present"), dict, List(Js.Num(1))),
      CompilerTest(Filter.selectKey("absent"), dict, List(Js.Null))
    )
  }

  val selectIndex: List[CompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2))
    List(CompilerTest(Filter.selectIndex(-3), arr, List(Js.Null)),
      CompilerTest(Filter.selectIndex(-2), arr, List(Js.Num(1))),
      CompilerTest(Filter.selectIndex(-1), arr, List(Js.Num(2))),
      CompilerTest(Filter.selectIndex(0), arr, List(Js.Num(1))),
      CompilerTest(Filter.selectIndex(1), arr, List(Js.Num(2))),
      CompilerTest(Filter.selectIndex(2), arr, List(Js.Null)))
  }

  val id: List[CompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(CompilerTest(Filter.id, dict, List(dict)))
  }

  val selectRange: List[CompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))
    List(
      CompilerTest(Filter.selectRange(0, 0), arr, List(Js.Arr())),
      CompilerTest(Filter.selectRange(0, 1), arr, List(Js.Arr(Js.Num(1)))),
      CompilerTest(Filter.selectRange(0, 2), arr, List(Js.Arr(Js.Num(1), Js.Num(2)))),
      CompilerTest(Filter.selectRange(0, 3), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3)))),
      CompilerTest(Filter.selectRange(0, 4), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      CompilerTest(Filter.selectRange(0, 5), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      CompilerTest(Filter.selectRange(1, 5), arr, List(Js.Arr(Js.Num(2), Js.Num(3), Js.Num(4))))
    )
  }

  val collectResults: List[CompilerTest] = {
    List(
      CompilerTest(Filter.collectResults(Filter.id),
        Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)),
        List(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))),
      CompilerTest(Filter.collectResults(Filter.id),
        Js.Obj("a" -> Js.Num(1), "b" -> Js.Str("c")),
        List(Js.Num(1), Js.Str("c")))
    )
  }

}
