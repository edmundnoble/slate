package qq

import upickle.Js

case class QQDoubleCompilerTest(program: QQFilter, input: Js.Value, expectedOutput: List[Js.Value])

object QQDoubleCompilerTest {

  val selectKeys: List[QQDoubleCompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(
      QQDoubleCompilerTest(QQFilter.selectKey("present"), dict, List(Js.Num(1))),
      QQDoubleCompilerTest(QQFilter.selectKey("absent"), dict, List(Js.Null))
    )
  }

  val selectIndex: List[QQDoubleCompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2))
    List(QQDoubleCompilerTest(QQFilter.selectIndex(-3), arr, List(Js.Null)),
      QQDoubleCompilerTest(QQFilter.selectIndex(-2), arr, List(Js.Num(1))),
      QQDoubleCompilerTest(QQFilter.selectIndex(-1), arr, List(Js.Num(2))),
      QQDoubleCompilerTest(QQFilter.selectIndex(0), arr, List(Js.Num(1))),
      QQDoubleCompilerTest(QQFilter.selectIndex(1), arr, List(Js.Num(2))),
      QQDoubleCompilerTest(QQFilter.selectIndex(2), arr, List(Js.Null)))
  }

  val id: List[QQDoubleCompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(QQDoubleCompilerTest(QQFilter.id, dict, List(dict)))
  }

  val selectRange: List[QQDoubleCompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))
    List(
      QQDoubleCompilerTest(QQFilter.selectRange(0, 0), arr, List(Js.Arr())),
      QQDoubleCompilerTest(QQFilter.selectRange(0, 1), arr, List(Js.Arr(Js.Num(1)))),
      QQDoubleCompilerTest(QQFilter.selectRange(0, 2), arr, List(Js.Arr(Js.Num(1), Js.Num(2)))),
      QQDoubleCompilerTest(QQFilter.selectRange(0, 3), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3)))),
      QQDoubleCompilerTest(QQFilter.selectRange(0, 4), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      QQDoubleCompilerTest(QQFilter.selectRange(0, 5), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      QQDoubleCompilerTest(QQFilter.selectRange(1, 5), arr, List(Js.Arr(Js.Num(2), Js.Num(3), Js.Num(4))))
    )
  }

  val collectResults: List[QQDoubleCompilerTest] = {
    List(
      QQDoubleCompilerTest(QQFilter.collectResults(QQFilter.id),
        Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)),
        List(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))),
      QQDoubleCompilerTest(QQFilter.collectResults(QQFilter.id),
        Js.Obj("a" -> Js.Num(1), "b" -> Js.Str("c")),
        List(Js.Num(1), Js.Str("c")))
    )
  }

}
