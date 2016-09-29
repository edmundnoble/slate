package qq

import monix.eval.Task
import monix.execution.Scheduler
import org.scalactic.{Normalization, Uniformity}
import org.scalatest.Assertion
import qq.cc.{JSONRuntime, QQCompiler, QQRuntime}
import qq.data.{ConcreteFilter, JSON}
import org.scalactic.NormMethods._

import scala.concurrent.Future

// data representation of a compiler test case
case class CompilerTest(input: JSON, program: ConcreteFilter, expectedOutput: JSON*)

object CompilerTest {

  import qq.data.QQDSL.fix._
  import org.scalatest.Matchers._

  implicit object canonicalized extends Uniformity[JSON] {
    final def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[JSON]
    final def normalizedOrSame(b: Any): Any =
      b match {
        case s: JSON => normalized(s)
        case _ => b
      }
    def normalized(right: JSON): JSON = toCanonical(right)
  }

  def toCanonical(j: JSON): JSON = j match {
    case (l: JSON.ObjList) => JSON.ObjList(l.value.map { case (k, v) => k -> toCanonical(v) })
    case (m: JSON.ObjMap) => toCanonical(m.toList)
    case JSON.Arr(values) => JSON.Arr(values.map(toCanonical))
    case _ => j
  }


  def runTest[J](runtime: QQRuntime[J], qqCompilerTest: CompilerTest)
                (implicit sch: Scheduler): Future[Assertion] = qqCompilerTest match {
    case CompilerTest(input, filter, expectedOutput@_*) =>
      QQCompiler
        .compileFilter(JSONRuntime, IndexedSeq.empty, filter)
        .fold[Task[Assertion]](
        err => Task.eval(fail("error occurred during compilation: \n" + err.toString)),
        program => program(Map.empty)(input).map { output =>
          output.map(_.norm) shouldBe expectedOutput.toList.map(_.norm)
          //          (output should be expectedOutput.toList) (after being canonicalized)
        })
        .runAsync
  }

  val selectKeyTests: List[CompilerTest] = {
    val dict = JSON.ObjList("present" -> JSON.Num(1))
    List(
      CompilerTest(dict, selectKey("present"), JSON.Num(1)),
      CompilerTest(dict, selectKey("absent"), JSON.Null)
    )
  }

  val selectIndexTests: List[CompilerTest] = {
    val arr = JSON.Arr(JSON.Num(1), JSON.Num(2))
    List(
      CompilerTest(arr, selectIndex(-3), JSON.Null),
      CompilerTest(arr, selectIndex(-2), JSON.Num(1)),
      CompilerTest(arr, selectIndex(-1), JSON.Num(2)),
      CompilerTest(arr, selectIndex(0), JSON.Num(1)),
      CompilerTest(arr, selectIndex(1), JSON.Num(2)),
      CompilerTest(arr, selectIndex(2), JSON.Null)
    )
  }

  val idTests: List[CompilerTest] = {
    val dict = JSON.ObjList("present" -> JSON.Num(1))
    List(CompilerTest(dict, id, dict))
  }

  val selectRangeTests: List[CompilerTest] = {
    val arr = JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4))
    List(
      CompilerTest(arr, selectRange(0, 0), JSON.Arr()),
      CompilerTest(arr, selectRange(0, 1), JSON.Arr(JSON.Num(1))),
      CompilerTest(arr, selectRange(0, 2), JSON.Arr(JSON.Num(1), JSON.Num(2))),
      CompilerTest(arr, selectRange(0, 3), JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3))),
      CompilerTest(arr, selectRange(0, 4), JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4))),
      CompilerTest(arr, selectRange(0, 5), JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4))),
      CompilerTest(arr, selectRange(1, 5), JSON.Arr(JSON.Num(2), JSON.Num(3), JSON.Num(4)))
    )
  }

  val collectResultsTests: List[CompilerTest] = {
    List(
      CompilerTest(JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4)), collectResults, JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4)),
      CompilerTest(JSON.ObjList("a" -> JSON.Num(1), "b" -> JSON.Str("c")), collectResults, JSON.Num(1), JSON.Str("c"))
    )
  }

  val fatStackTests: List[CompilerTest] = {
    // tail recursive builder, giving you (i * 2) compositions of id with f.
    @annotation.tailrec
    def fun(f: ConcreteFilter, i: Int): ConcreteFilter = if (i == 0) f else fun(id | f | id, i - 1)
    List(
      CompilerTest(JSON.False, fun(id, 1000), JSON.False)
    )
  }

  val variableTests: List[CompilerTest] =
    List(
      CompilerTest(JSON.Str("input"), asBinding("hello", id, deref("hello")), JSON.Str("input")),
      CompilerTest(JSON.Str("input"), asBinding("hello", add(id, constString("hi")), add(constString("hey"), deref("hello"))), JSON.Str("heyinputhi"))
    )

  val pathSetterTests: List[CompilerTest] =
    List(
      CompilerTest(JSON.Num(2), setPath(Nil, constNumber(3)), JSON.Num(3)),
      CompilerTest(
        JSON.Arr(
          JSON.ObjList("key1" -> JSON.ObjList("key2" -> JSON.Str("input1")), "out1" -> JSON.Str("output1")),
          JSON.ObjList("key1" -> JSON.ObjList("key2" -> JSON.Str("input2")), "out1" -> JSON.Str("output2"))
        ),
        getPathS(collectResults) | setPath(List(selectKey("key1"), selectKey("key2")), getPathS(selectKey("out1"))),
        JSON.ObjList("key1" -> JSON.ObjList("key2" -> JSON.Str("output1")), "out1" -> JSON.Str("output1")),
        JSON.ObjList("key1" -> JSON.ObjList("key2" -> JSON.Str("output2")), "out1" -> JSON.Str("output2"))
      )
    )

  val pathModifierTests: List[CompilerTest] =
    List(
      CompilerTest(JSON.Num(2), modifyPath(Nil, constNumber(3)), JSON.Num(3)),
      CompilerTest(
        JSON.Arr(
          JSON.ObjList("key1" -> JSON.ObjList("out1" -> JSON.Str("output1"))),
          JSON.ObjList("key1" -> JSON.ObjList("out1" -> JSON.Str("output2")))
        ),
        modifyPath(List(collectResults, selectKey("key1"), selectKey("out1")), add(id, constString("mod"))),
        JSON.ObjList("key1" -> JSON.ObjList("out1" -> JSON.Str("output1mod"))),
        JSON.ObjList("key1" -> JSON.ObjList("out1" -> JSON.Str("output2mod")))
      ),
      CompilerTest(
        JSON.Arr(
          JSON.ObjList("key1" -> JSON.ObjList("out1" -> JSON.Str("output1"))),
          JSON.ObjList("key1" -> JSON.ObjList("out1" -> JSON.Str("output2")))
        ),
        modifyPath(List(collectResults, selectKey("key1")), getPathS(selectKey("out1"))),
        JSON.ObjList("key1" -> JSON.Str("output1")),
        JSON.ObjList("key1" -> JSON.Str("output2"))
      ),
      CompilerTest(
        JSON.Arr(
          JSON.ObjList("out1" -> JSON.Str("output1")),
          JSON.ObjList("out1" -> JSON.Str("output2"))
        ),
        modifyPath(List(selectIndex(0)), getPathS(selectKey("out1"))),
        JSON.Arr(
          JSON.Str("output1"),
          JSON.ObjList("out1" -> JSON.Str("output2"))
        )
      )
    )


}
