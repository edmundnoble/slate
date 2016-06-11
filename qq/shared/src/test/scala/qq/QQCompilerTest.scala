package qq

import qq.QQAST._
import upickle.Js
import utest._


case class QQDoubleCompilerTest(program: QQProgram, input: Js.Value, expectedOutput: List[Js.Value])

object QQDoubleCompilerTest {

  import QQAST.QQFilter

  val selectKeys: List[QQDoubleCompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(
      QQDoubleCompilerTest(QQProgram.selectKey("present"), dict, List(Js.Num(1))),
      QQDoubleCompilerTest(QQProgram.selectKey("absent"), dict, List(Js.Null))
    )
  }

  val selectIndex: List[QQDoubleCompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2))
    List(QQDoubleCompilerTest(QQProgram.selectIndex(-3), arr, List(Js.Null)),
      QQDoubleCompilerTest(QQProgram.selectIndex(-2), arr, List(Js.Num(1))),
      QQDoubleCompilerTest(QQProgram.selectIndex(-1), arr, List(Js.Num(2))),
      QQDoubleCompilerTest(QQProgram.selectIndex(0), arr, List(Js.Num(1))),
      QQDoubleCompilerTest(QQProgram.selectIndex(1), arr, List(Js.Num(2))),
      QQDoubleCompilerTest(QQProgram.selectIndex(2), arr, List(Js.Null)))
  }

  val id: List[QQDoubleCompilerTest] = {
    val dict = Js.Obj("present" -> Js.Num(1))
    List(QQDoubleCompilerTest(QQProgram.id, dict, List(dict)))
  }

  val selectRange: List[QQDoubleCompilerTest] = {
    val arr = Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))
    List(
      QQDoubleCompilerTest(QQProgram.selectRange(0, 0), arr, List(Js.Arr())),
      QQDoubleCompilerTest(QQProgram.selectRange(0, 1), arr, List(Js.Arr(Js.Num(1)))),
      QQDoubleCompilerTest(QQProgram.selectRange(0, 2), arr, List(Js.Arr(Js.Num(1), Js.Num(2)))),
      QQDoubleCompilerTest(QQProgram.selectRange(0, 3), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3)))),
      QQDoubleCompilerTest(QQProgram.selectRange(0, 4), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      QQDoubleCompilerTest(QQProgram.selectRange(0, 5), arr, List(Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)))),
      QQDoubleCompilerTest(QQProgram.selectRange(1, 5), arr, List(Js.Arr(Js.Num(2), Js.Num(3), Js.Num(4))))
    )
  }

  val collectResults: List[QQDoubleCompilerTest] = {
    List(
      QQDoubleCompilerTest(QQProgram.collectResults(QQProgram.id),
        Js.Arr(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4)),
        List(Js.Num(1), Js.Num(2), Js.Num(3), Js.Num(4))),
      QQDoubleCompilerTest(QQProgram.collectResults(QQProgram.id),
        Js.Obj("a" -> Js.Num(1), "b" -> Js.Str("c")),
        List(Js.Num(1), Js.Str("c")))
    )
  }

}
