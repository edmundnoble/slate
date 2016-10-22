package qq

import monix.eval.Task
import org.scalactic.NormMethods._
import org.scalatest.Assertion
import qq.cc.{CompiledFilter, QQCompiler}
import qq.data.{ConcreteFilter, JSON, QQDSL}

import scala.concurrent.Future

// data representation of a compiler test case
case class CompilerTestCase(input: JSON, program: ConcreteFilter, expectedOutput: JSON*)

class CompilerTest extends QQAsyncTestSuite {

  import qq.data.QQDSL._

  def runTest(qqCompilerTest: CompilerTestCase): Future[Assertion] = qqCompilerTest match {
    case CompilerTestCase(input, filter, expectedOutput@_*) =>
      QQCompiler
        .compileFilter(Vector.empty, filter)
        .fold[Task[Assertion]](
        err => Task.eval(fail("error occurred during compilation: \n" + err.toString)),
        program => CompiledFilter.run(input, Map.empty, program).map { output =>
          output.value.map(_.norm) shouldBe expectedOutput.toList.map(_.norm)
        })
        .runAsync
  }

  val selectKeyTests: List[CompilerTestCase] = {
    val dict = JSON.Obj("present" -> JSON.Num(1))
    List(
      CompilerTestCase(dict, selectKey("present"), JSON.Num(1)),
      CompilerTestCase(dict, selectKey("absent"), JSON.Null)
    )
  }

  val selectIndexTests: List[CompilerTestCase] = {
    val arr = JSON.Arr(JSON.Num(1), JSON.Num(2))
    List(
      CompilerTestCase(arr, selectIndex(-3), JSON.Null),
      CompilerTestCase(arr, selectIndex(-2), JSON.Num(1)),
      CompilerTestCase(arr, selectIndex(-1), JSON.Num(2)),
      CompilerTestCase(arr, selectIndex(0), JSON.Num(1)),
      CompilerTestCase(arr, selectIndex(1), JSON.Num(2)),
      CompilerTestCase(arr, selectIndex(2), JSON.Null)
    )
  }

  val idTests: List[CompilerTestCase] = {
    val dict = JSON.Obj("present" -> JSON.Num(1))
    List(CompilerTestCase(dict, id, dict))
  }

  val selectRangeTests: List[CompilerTestCase] = {
    val arr = JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4))
    List(
      CompilerTestCase(arr, selectRange(0, 0), JSON.Arr()),
      CompilerTestCase(arr, selectRange(0, 1), JSON.Arr(JSON.Num(1))),
      CompilerTestCase(arr, selectRange(0, 2), JSON.Arr(JSON.Num(1), JSON.Num(2))),
      CompilerTestCase(arr, selectRange(0, 3), JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3))),
      CompilerTestCase(arr, selectRange(0, 4), JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4))),
      CompilerTestCase(arr, selectRange(0, 5), JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4))),
      CompilerTestCase(arr, selectRange(1, 5), JSON.Arr(JSON.Num(2), JSON.Num(3), JSON.Num(4)))
    )
  }

  val collectResultsTests: List[CompilerTestCase] = {
    List(
      CompilerTestCase(JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4)), collectResults, JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4)),
      CompilerTestCase(JSON.Obj("a" -> JSON.Num(1), "b" -> JSON.Str("c")), collectResults, JSON.Num(1), JSON.Str("c"))
    )
  }

  val fatStackTests: List[CompilerTestCase] = {
    // tail recursive builder, giving you (i * 2) compositions of id with f.
    @annotation.tailrec
    def fun(f: ConcreteFilter, i: Int): ConcreteFilter = if (i == 0) f else fun(id | f | id, i - 1)
    List(
      CompilerTestCase(JSON.False, fun(id, 1000), JSON.False)
    )
  }

  val variableTests: List[CompilerTestCase] =
    List(
      CompilerTestCase(JSON.Str("input"), asBinding("hello", id, deref("hello")), JSON.Str("input")),
      CompilerTestCase(JSON.Str("input"), asBinding("hello", add(id, constString("hi")), add(constString("hey"), deref("hello"))), JSON.Str("heyinputhi"))
    )

  val pathSetterTests: List[CompilerTestCase] =
    List(
      CompilerTestCase(JSON.Num(2), setPath(Nil, constNumber(3)), JSON.Num(3)),
      CompilerTestCase(
        JSON.Arr(
          JSON.Obj("key1" -> JSON.Obj("key2" -> JSON.Str("input1")), "out1" -> JSON.Str("output1")),
          JSON.Obj("key1" -> JSON.Obj("key2" -> JSON.Str("input2")), "out1" -> JSON.Str("output2"))
        ),
        getPathS(collectResults) | setPath(List(selectKey("key1"), selectKey("key2")), getPathS(selectKey("out1"))),
        JSON.Obj("key1" -> JSON.Obj("key2" -> JSON.Str("output1")), "out1" -> JSON.Str("output1")),
        JSON.Obj("key1" -> JSON.Obj("key2" -> JSON.Str("output2")), "out1" -> JSON.Str("output2"))
      )
    )

  val pathModifierTests: List[CompilerTestCase] =
    List(
      CompilerTestCase(JSON.Num(2), modifyPath(Nil, constNumber(3)), JSON.Num(3)),
      CompilerTestCase(
        JSON.Arr(
          JSON.Obj("key1" -> JSON.Obj("out1" -> JSON.Str("output1"))),
          JSON.Obj("key1" -> JSON.Obj("out1" -> JSON.Str("output2")))
        ),
        modifyPath(List(collectResults, selectKey("key1"), selectKey("out1")), add(id, constString("mod"))),
        JSON.Obj("key1" -> JSON.Obj("out1" -> JSON.Str("output1mod"))),
        JSON.Obj("key1" -> JSON.Obj("out1" -> JSON.Str("output2mod")))
      ),
      CompilerTestCase(
        JSON.Arr(
          JSON.Obj("key1" -> JSON.Obj("out1" -> JSON.Str("output1"))),
          JSON.Obj("key1" -> JSON.Obj("out1" -> JSON.Str("output2")))
        ),
        modifyPath(List(collectResults, selectKey("key1")), getPathS(selectKey("out1"))),
        JSON.Obj("key1" -> JSON.Str("output1")),
        JSON.Obj("key1" -> JSON.Str("output2"))
      ),
      CompilerTestCase(
        JSON.Arr(
          JSON.Obj("out1" -> JSON.Str("output1")),
          JSON.Obj("out1" -> JSON.Str("output2"))
        ),
        modifyPath(List(selectIndex(0)), getPathS(selectKey("out1"))),
        JSON.Arr(
          JSON.Str("output1"),
          JSON.Obj("out1" -> JSON.Str("output2"))
        )
      )
    )

  val mathTests = {
    val obj = JSON.Obj("fst" -> JSON.Num(1), "snd" -> JSON.Num(2))
    List(
      CompilerTestCase(obj, add(selectKey("fst"), selectKey("snd")), JSON.Num(3)),
      CompilerTestCase(obj, multiply(selectKey("fst"), selectKey("snd")), JSON.Num(2)),
      CompilerTestCase(obj, divide(selectKey("fst"), selectKey("snd")), JSON.Num(0.5)),
      CompilerTestCase(obj, modulo(selectKey("fst"), selectKey("snd")), JSON.Num(1)),
      CompilerTestCase(obj, QQDSL.equal(selectKey("fst"), selectKey("snd")), JSON.False),
      CompilerTestCase(obj, QQDSL.lte(selectKey("fst"), selectKey("snd")), JSON.True),
      CompilerTestCase(obj, QQDSL.gte(selectKey("fst"), selectKey("snd")), JSON.False),
      CompilerTestCase(obj, lessThan(selectKey("fst"), selectKey("snd")), JSON.True),
      CompilerTestCase(obj, greaterThan(selectKey("fst"), selectKey("snd")), JSON.False),
      CompilerTestCase(obj, multiply("fst", 0), JSON.Null),
      CompilerTestCase(obj, multiply("fst", 2), JSON.Str("fstfst"))
    )
  }

  "select keys" in Future.traverse(selectKeyTests)(runTest)
  "select index" in Future.traverse(selectIndexTests)(runTest)
  "id" in Future.traverse(idTests)(runTest)
  "select range" in Future.traverse(selectRangeTests)(runTest)
  "collect results" in Future.traverse(collectResultsTests)(runTest)
  "fat stack" taggedAs StackTest in Future.traverse(fatStackTests)(runTest)
  "variables" in Future.traverse(variableTests)(runTest)
  "path setters" in Future.traverse(pathSetterTests)(runTest)
  "path modifiers" in Future.traverse(pathModifierTests)(runTest)
  "math" in Future.traverse(mathTests)(runTest)

}
