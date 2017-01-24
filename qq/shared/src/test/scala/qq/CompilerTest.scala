package qq

import cats.Eval
import cats.implicits._
import monix.eval.Task
import org.scalatest.Assertion
import qq.cc.{InterpretedFilter, QQCompiler, QQRuntimeException, RuntimeErrs}
import qq.data.{FilterAST, JSON, QQDSL, SelectKey}
import qq.util.Recursion.RecursionEngine

import scala.concurrent.Future

// data representation of a compiler test case
case class CompilerTestCase(input: JSON, program: FilterAST, expectedOutput: Either[RuntimeErrs, Vector[JSON]])(implicit val recEngine: RecursionEngine)

class CompilerTest extends QQAsyncTestSuite {

  import qq.data.QQDSL._

  def runTest(qqCompilerTest: CompilerTestCase): Future[Assertion] = qqCompilerTest match {
    case CompilerTestCase(input, filter, expectedOutput) =>
      QQCompiler
        .compileFilter(Vector.empty, filter)(qqCompilerTest.recEngine)
        .fold[Task[Assertion]](
        err => Task.eval(fail("error occurred during compilation: \n" + err.toString)),
        program => CompiledFilter.run(input, Map.empty, program).map { output =>
          output.map(_.map(_.norm)) shouldBe expectedOutput.map(_.map(_.norm))
        })
        .runAsync
  }

  val selectKeyTests: Vector[CompilerTestCase] = {
    val dict = JSON.Obj("present" -> JSON.Num(1))
    Vector(
      CompilerTestCase(dict, selectKey("present"), Either.right(Vector(JSON.Num(1)))),
      CompilerTestCase(dict, selectKey("absent"), Either.right(Vector(JSON.Null)))
    )
  }

  val selectIndexTests: Vector[CompilerTestCase] = {
    val arr = JSON.Arr(JSON.Num(1), JSON.Num(2))
    Vector(
      CompilerTestCase(arr, selectIndex(-3), Either.right(Vector(JSON.Null))),
      CompilerTestCase(arr, selectIndex(-2), Either.right(Vector(JSON.Num(1)))),
      CompilerTestCase(arr, selectIndex(-1), Either.right(Vector(JSON.Num(2)))),
      CompilerTestCase(arr, selectIndex(0), Either.right(Vector(JSON.Num(1)))),
      CompilerTestCase(arr, selectIndex(1), Either.right(Vector(JSON.Num(2)))),
      CompilerTestCase(arr, selectIndex(2), Either.right(Vector(JSON.Null)))
    )
  }

  val idTests: Vector[CompilerTestCase] = {
    val dict = JSON.Obj("present" -> JSON.Num(1))
    Vector(CompilerTestCase(dict, id, Either.right(Vector(dict))))
  }

  val selectRangeTests: Vector[CompilerTestCase] = {
    val arr = JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4))
    Vector(
      CompilerTestCase(arr, selectRange(0, 0), Either.right(Vector(JSON.Arr()))),
      CompilerTestCase(arr, selectRange(0, 1), Either.right(Vector(JSON.Arr(JSON.Num(1))))),
      CompilerTestCase(arr, selectRange(0, 2), Either.right(Vector(JSON.Arr(JSON.Num(1), JSON.Num(2))))),
      CompilerTestCase(arr, selectRange(0, 3), Either.right(Vector(JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3))))),
      CompilerTestCase(arr, selectRange(0, 4), Either.right(Vector(JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4))))),
      CompilerTestCase(arr, selectRange(0, 5), Either.right(Vector(JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4))))),
      CompilerTestCase(arr, selectRange(1, 5), Either.right(Vector(JSON.Arr(JSON.Num(2), JSON.Num(3), JSON.Num(4)))))
    )
  }

  val collectResultsTests: Vector[CompilerTestCase] = {
    Vector(
      CompilerTestCase(JSON.Arr(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4)), collectResults, Either.right(Vector(JSON.Num(1), JSON.Num(2), JSON.Num(3), JSON.Num(4)))),
      CompilerTestCase(JSON.obj("a" -> JSON.Num(1), "b" -> JSON.Str("c")), collectResults, Either.right(Vector(JSON.Num(1), JSON.Str("c"))))
    )
  }

  val lotsOfCompositionTests: Vector[CompilerTestCase] = {
    // tail recursive builder, giving you (i * 2) compositions of id with f.
    @annotation.tailrec
    def fun(f: FilterAST, i: Int): FilterAST = if (i == 0) f else fun(id | f | id, i - 1)
    Vector(
      CompilerTestCase(JSON.False, fun(id, 10000), Either.right(Vector(JSON.False)))(qq.Platform.Rec.defaultRecScheme)
    )
  }

  val lotsOfEnsequenceTests: Vector[CompilerTestCase] = {
    // tail recursive builder, giving you (i * 2) ensequencings of id with f.
    @annotation.tailrec
    def fun(f: FilterAST, i: Int): FilterAST = if (i == 0) f else fun(id |+| f |+| id, i - 1)
    Vector(
      CompilerTestCase(JSON.False, fun(id, 10000), Either.right(Vector.fill(20001)(JSON.`false`)))(qq.Platform.Rec.defaultRecScheme)
    )
  }

  val lotsOfSelectKeyTests: Vector[CompilerTestCase] = {
    // tail recursive builders, giving you i invocations of
    // selectKey composed both inside and outside of a single path component
    @annotation.tailrec
    def funSlow(f: FilterAST, i: Int): FilterAST = if (i == 0) f else funSlow(f | getPathS(SelectKey("key")), i - 1)
    def funFast(f: FilterAST, i: Int): FilterAST = f | getPath(Vector.fill(i)(SelectKey("key")))
    def nest(obj: JSON, i: Int): Eval[JSON] =
      if (i == 0) Eval.now(obj)
      else Eval.defer(nest(obj, i - 1)).map(r => JSON.obj("key" -> r))
    val input = nest(JSON.False, 10000).value
    Vector(
      CompilerTestCase(input, funFast(id, 10000), Either.right(Vector(JSON.`false`)))(qq.Platform.Rec.defaultRecScheme),
      CompilerTestCase(input, funSlow(id, 10000), Either.right(Vector(JSON.`false`)))(qq.Platform.Rec.defaultRecScheme)
    )
  }

  val variableTests: Vector[CompilerTestCase] =
    Vector(
      CompilerTestCase(JSON.Str("input"), asBinding("hello", id, deref("hello")), Either.right(Vector(JSON.Str("input")))),
      CompilerTestCase(JSON.Str("input"), asBinding("hello", add(id, constString("hi")), add(constString("hey"), deref("hello"))), Either.right(Vector(JSON.Str("heyinputhi"))))
    )

  val pathSetterTests: Vector[CompilerTestCase] =
    Vector(
      CompilerTestCase(JSON.Num(2), setPath(Vector.empty, constNumber(3)), Either.right(Vector(JSON.Num(3)))),
      CompilerTestCase(JSON.arr(JSON.num(1), JSON.num(2)), setPath(Vector(selectIndex(1)), constNumber(1)), Either.right(Vector(JSON.arr(JSON.num(1), JSON.num(1))))),
      CompilerTestCase(JSON.arr(JSON.num(1), JSON.num(2)), setPath(Vector(selectIndex(2)), constNumber(1)), Either.right(Vector(JSON.arr(JSON.num(1), JSON.num(2), JSON.num(1))))),
      CompilerTestCase(JSON.arr(JSON.num(1), JSON.num(2)), setPath(Vector(selectIndex(3)), constNumber(1)), Either.right(Vector(JSON.arr(JSON.num(1), JSON.num(2), JSON.`null`, JSON.num(1))))),
      CompilerTestCase(
        JSON.Arr(
          JSON.Obj("key1" -> JSON.Obj("key2" -> JSON.Str("input1")), "out1" -> JSON.Str("output1")),
          JSON.Obj("key1" -> JSON.Obj("key2" -> JSON.Str("input2")), "out1" -> JSON.Str("output2"))
        ),
        getPathS(collectResults) | setPath(Vector(selectKey("key1"), selectKey("key2")), getPathS(selectKey("out1"))),
        Either.right(Vector(JSON.obj("key1" -> JSON.Obj("key2" -> JSON.Str("output1")), "out1" -> JSON.Str("output1")),
          JSON.Obj("key1" -> JSON.Obj("key2" -> JSON.Str("output2")), "out1" -> JSON.Str("output2"))))
      ),
      CompilerTestCase(
        JSON.num(2),
        setPath(Vector(collectResults), getPath(Vector.empty)),
        QQRuntimeException.typeError("collect results from", "array" -> JSON.num(2))
      ),
      CompilerTestCase(
        JSON.num(2),
        setPath(Vector(selectKey("key")), getPath(Vector.empty)),
        QQRuntimeException.typeError("select key \"key\" in", "object" -> JSON.num(2))
      ),
      CompilerTestCase(
        JSON.num(2),
        setPath(Vector(selectIndex(2)), getPath(Vector.empty)),
        QQRuntimeException.typeError("select index 2 in", "array" -> JSON.num(2))
      )
    )

  val pathModifierTests: Vector[CompilerTestCase] =
    Vector(
      CompilerTestCase(JSON.Num(2), modifyPath(Vector.empty, constNumber(3)), Either.right(Vector(JSON.Num(3)))),
      CompilerTestCase(
        JSON.Arr(
          JSON.Obj("key1" -> JSON.Obj("out1" -> JSON.Str("output1"))),
          JSON.Obj("key1" -> JSON.Obj("out1" -> JSON.Str("output2")))
        ),
        modifyPath(Vector(collectResults, selectKey("key1"), selectKey("out1")), add(id, constString("mod"))),
        Either.right(Vector(JSON.obj("key1" -> JSON.Obj("out1" -> JSON.Str("output1mod"))),
          JSON.obj("key1" -> JSON.Obj("out1" -> JSON.Str("output2mod")))))
      ),
      CompilerTestCase(
        JSON.Arr(
          JSON.Obj("key1" -> JSON.Obj("out1" -> JSON.Str("output1"))),
          JSON.Obj("key1" -> JSON.Obj("out1" -> JSON.Str("output2")))
        ),
        modifyPath(Vector(collectResults, selectKey("key1")), getPathS(selectKey("out1"))),
        Either.right(Vector(JSON.Obj("key1" -> JSON.Str("output1")),
          JSON.Obj("key1" -> JSON.Str("output2"))))
      ),
      CompilerTestCase(
        JSON.Arr(
          JSON.Obj("out1" -> JSON.Str("output1")),
          JSON.Obj("out1" -> JSON.Str("output2"))
        ),
        modifyPath(Vector(selectIndex(0)), getPathS(selectKey("out1"))),
        Either.right(Vector(JSON.Arr(
          JSON.Str("output1"),
          JSON.Obj("out1" -> JSON.Str("output2"))
        )))
      ),
      CompilerTestCase(
        JSON.num(2),
        modifyPath(Vector(collectResults), getPath(Vector.empty)),
        QQRuntimeException.typeError("collect results from", "array" -> JSON.num(2))
      ),
      CompilerTestCase(
        JSON.num(2),
        modifyPath(Vector(selectKey("key")), getPath(Vector.empty)),
        QQRuntimeException.typeError("select key \"key\" in", "object" -> JSON.num(2))
      ),
      CompilerTestCase(
        JSON.num(2),
        modifyPath(Vector(selectIndex(2)), getPath(Vector.empty)),
        QQRuntimeException.typeError("select index 2 in", "array" -> JSON.num(2))
      )
    )

  val mathTests = {
    val obj = JSON.Obj("fst" -> JSON.Num(1), "snd" -> JSON.Num(2))
    Vector(
      CompilerTestCase(obj, add(selectKey("fst"), selectKey("snd")), Either.right(Vector(JSON.Num(3)))),
      CompilerTestCase(obj, multiply(selectKey("fst"), selectKey("snd")), Either.right(Vector(JSON.Num(2)))),
      CompilerTestCase(obj, divide(selectKey("fst"), selectKey("snd")), Either.right(Vector(JSON.Num(0.5)))),
      CompilerTestCase(obj, modulo(selectKey("fst"), selectKey("snd")), Either.right(Vector(JSON.Num(1)))),
      CompilerTestCase(obj, QQDSL.equal(selectKey("fst"), selectKey("snd")), Either.right(Vector(JSON.False))),
      CompilerTestCase(obj, QQDSL.lte(selectKey("fst"), selectKey("snd")), Either.right(Vector(JSON.True))),
      CompilerTestCase(obj, QQDSL.gte(selectKey("fst"), selectKey("snd")), Either.right(Vector(JSON.False))),
      CompilerTestCase(obj, lessThan(selectKey("fst"), selectKey("snd")), Either.right(Vector(JSON.True))),
      CompilerTestCase(obj, greaterThan(selectKey("fst"), selectKey("snd")), Either.right(Vector(JSON.False))),
      CompilerTestCase(obj, multiply("fst", 0), Either.right(Vector(JSON.Null))),
      CompilerTestCase(obj, multiply("fst", 2), Either.right(Vector(JSON.Str("fstfst"))))
    )
  }

  "select keys" in Future.traverse(selectKeyTests)(runTest)
  "select index" in Future.traverse(selectIndexTests)(runTest)
  "id" in Future.traverse(idTests)(runTest)
  "select range" in Future.traverse(selectRangeTests)(runTest)
  "collect results" in Future.traverse(collectResultsTests)(runTest)
  "lots of compositions" taggedAs StackTest in Future.traverse(lotsOfCompositionTests)(runTest)
  "lots of ensequence" taggedAs StackTest in Future.traverse(lotsOfEnsequenceTests)(runTest)
  "lots of selectKey" taggedAs StackTest in Future.traverse(lotsOfSelectKeyTests)(runTest)
  "variables" in Future.traverse(variableTests)(runTest)
  "path setters" in Future.traverse(pathSetterTests)(runTest)
  "path modifiers" in Future.traverse(pathModifierTests)(runTest)
  "math" in Future.traverse(mathTests)(runTest)

}
