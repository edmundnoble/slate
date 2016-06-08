package qq

import qq.QQAST._
import upickle.Js
import utest._


case class QQDoubleCompilerTest(program: QQFilter, input: Js.Value, expectedOutput: List[Js.Value])

object QQDoubleCompilerTest {

  val selectKeys: List[QQDoubleCompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(
      QQDoubleCompilerTest(SelectKey("present"), dict, List(Js.Num(1))),
      QQDoubleCompilerTest(SelectKey("absent"), dict, List(Js.Null))
    )
  }

  val selectIndex: List[QQDoubleCompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2))
    List(QQDoubleCompilerTest(SelectIndex(-3), arr, List(Js.Null)),
      QQDoubleCompilerTest(SelectIndex(-2), arr, List(Js.Num(1))),
      QQDoubleCompilerTest(SelectIndex(-1), arr, List(Js.Num(2))),
      QQDoubleCompilerTest(SelectIndex(0), arr, List(Js.Num(1))),
      QQDoubleCompilerTest(SelectIndex(1), arr, List(Js.Num(2))),
      QQDoubleCompilerTest(SelectIndex(2), arr, List(Js.Null)))
  }

  val id: List[QQDoubleCompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(QQDoubleCompilerTest(IdFilter, dict, List(dict)))
  }

  val selectRange: List[QQDoubleCompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))
    List(
      QQDoubleCompilerTest(SelectRange(0, 0), arr, List(Js.Arr())),
      QQDoubleCompilerTest(SelectRange(0, 1), arr, List(Js.Arr(Js.Num(1)))),
      QQDoubleCompilerTest(SelectRange(0, 2), arr, List(Js.Arr(Js.Num(1), Js.Num(2)))),
      QQDoubleCompilerTest(SelectRange(0, 3), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3)))),
      QQDoubleCompilerTest(SelectRange(0, 4), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      QQDoubleCompilerTest(SelectRange(0, 5), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      QQDoubleCompilerTest(SelectRange(1, 5), arr, List(Js.Arr(Js.Num(2), Js.Num(3), Js.Num(4))))
    )
  }

  val collectResults: List[QQDoubleCompilerTest] = {
    List(
      QQDoubleCompilerTest(CollectResults(IdFilter),
        Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)),
        List(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))),
      QQDoubleCompilerTest(CollectResults(IdFilter),
        Js.Obj("a" -> Js.Num(1), "b" -> Js.Str("c")),
        List(Js.Num(1), Js.Str("c")))
    )
  }

}
