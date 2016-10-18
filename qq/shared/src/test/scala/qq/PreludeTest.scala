package qq

import org.scalactic.NormMethods._
import org.scalatest.Assertion
import qq.cc.{QQRuntimeError, Runner}
import qq.data.JSON

import scala.concurrent.Future
import scalaz.{ValidationNel, \/}
import scalaz.std.list._
import scalaz.std.scalaFuture._
import scalaz.syntax.validation._
import scalaz.syntax.std.`try`._
import scalaz.syntax.traverse._

case class PreludeTestCase(input: JSON, program: String, expectedOutputOrException: ValidationNel[QQRuntimeError, List[JSON]])

class PreludeTest extends QQAsyncTestSuite {

  def runTest(test: PreludeTestCase): Future[Assertion] =
    Runner
      .run(test.program)(List(test.input))
      .value
      .map(_.map(_.map(_.norm)) should be(test.expectedOutputOrException.map(_.map(_.norm))))
      .runAsync

  val selectTests: List[PreludeTestCase] = {
    val first: JSON =
      JSON.Obj("id" -> JSON.Str("first"), "val" -> JSON.Num(1))
    val second: JSON =
      JSON.Obj("id" -> JSON.Str("second"), "val" -> JSON.Num(2))
    List(
      PreludeTestCase(first, "select(.id == \"second\")", Nil.successNel),
      PreludeTestCase(second, "select(.id == \"second\")", List(second).successNel)
    )
  }

  val classifierTestInput =
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False)

  val numbers = PreludeTestCase(classifierTestInput, ".[] | numbers", List(JSON.Num(1)).successNel)

  val arrays = PreludeTestCase(classifierTestInput, ".[] | arrays", List(JSON.Arr()).successNel)

  val objects = PreludeTestCase(classifierTestInput, ".[] | objects", List(JSON.Obj()).successNel)

  val iterables = PreludeTestCase(classifierTestInput, ".[] | iterables", List(JSON.Arr(), JSON.Obj()).successNel)

  val booleans = PreludeTestCase(classifierTestInput, ".[] | booleans", List(JSON.True, JSON.False).successNel)

  val strings = PreludeTestCase(classifierTestInput, ".[] | strings", List(JSON.Str("foo")).successNel)

  val nulls = PreludeTestCase(classifierTestInput, ".[] | nulls", List(JSON.Null).successNel)

  val values = PreludeTestCase(
    classifierTestInput,
    ".[] | values",
    List(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.True, JSON.False).successNel
  )

  val scalars = PreludeTestCase(
    classifierTestInput,
    ".[] | scalars",
    List(JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False).successNel
  )

  "select" in selectTests.traverse(runTest)
  "classifiers" - {
    "arrays" in runTest(arrays)
    "strings" in runTest(strings)
    "booleans" in runTest(booleans)
    "scalars" in runTest(scalars)
    "objects" in runTest(objects)
    "iterables" in runTest(iterables)
    "nulls" in runTest(nulls)
    "numbers" in runTest(numbers)
  }
}
