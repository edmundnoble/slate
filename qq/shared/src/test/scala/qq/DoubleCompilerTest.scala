package qq

import upickle.Js

case class DoubleCompilerTest(program: Filter, input: Js.Value, expectedOutput: List[Js.Value])

object DoubleCompilerTest {

  val selectKeys: List[DoubleCompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(
      DoubleCompilerTest(Filter.selectKey("present"), dict, List(Js.Num(1))),
      DoubleCompilerTest(Filter.selectKey("absent"), dict, List(Js.Null))
    )
  }

  val selectIndex: List[DoubleCompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2))
    List(DoubleCompilerTest(Filter.selectIndex(-3), arr, List(Js.Null)),
      DoubleCompilerTest(Filter.selectIndex(-2), arr, List(Js.Num(1))),
      DoubleCompilerTest(Filter.selectIndex(-1), arr, List(Js.Num(2))),
      DoubleCompilerTest(Filter.selectIndex(0), arr, List(Js.Num(1))),
      DoubleCompilerTest(Filter.selectIndex(1), arr, List(Js.Num(2))),
      DoubleCompilerTest(Filter.selectIndex(2), arr, List(Js.Null)))
  }

  val id: List[DoubleCompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(DoubleCompilerTest(Filter.id, dict, List(dict)))
  }

  val selectRange: List[DoubleCompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))
    List(
      DoubleCompilerTest(Filter.selectRange(0, 0), arr, List(Js.Arr())),
      DoubleCompilerTest(Filter.selectRange(0, 1), arr, List(Js.Arr(Js.Num(1)))),
      DoubleCompilerTest(Filter.selectRange(0, 2), arr, List(Js.Arr(Js.Num(1), Js.Num(2)))),
      DoubleCompilerTest(Filter.selectRange(0, 3), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3)))),
      DoubleCompilerTest(Filter.selectRange(0, 4), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      DoubleCompilerTest(Filter.selectRange(0, 5), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      DoubleCompilerTest(Filter.selectRange(1, 5), arr, List(Js.Arr(Js.Num(2), Js.Num(3), Js.Num(4))))
    )
  }

  val collectResults: List[DoubleCompilerTest] = {
    List(
      DoubleCompilerTest(Filter.collectResults(Filter.id),
        Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)),
        List(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))),
      DoubleCompilerTest(Filter.collectResults(Filter.id),
        Js.Obj("a" -> Js.Num(1), "b" -> Js.Str("c")),
        List(Js.Num(1), Js.Str("c")))
    )
  }

}
