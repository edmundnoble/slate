package qq

import org.scalactic.NormMethods._
import org.scalatest.Assertion
import qq.cc.{JSONRuntime, Runner}
import qq.data.JSON

import scala.concurrent.Future
import scalaz.\/
import scalaz.std.list._
import scalaz.std.scalaFuture._
import scalaz.syntax.either._
import scalaz.syntax.std.`try`._
import scalaz.syntax.traverse._

case class PreludeTestCase(input: JSON, program: String, expectedOutputOrException: Exception \/ List[JSON])

class PreludeTest extends QQAsyncTestSuite {

  def runTest(test: PreludeTestCase): Future[Assertion] =
    Runner
      .run(JSONRuntime, test.program)(List(test.input))
      .materialize
      .map(_.map(_.map(_.norm)).toDisjunction should be(test.expectedOutputOrException.map(_.map(_.norm))))
      .runAsync

  val selectTests: List[PreludeTestCase] = {
    val first: JSON =
      JSON.Obj("id" -> JSON.Str("first"), "val" -> JSON.Num(1))
    val second: JSON =
      JSON.Obj("id" -> JSON.Str("second"), "val" -> JSON.Num(2))
    List(
      PreludeTestCase(first, "select(.id == \"second\")", Nil.right),
      PreludeTestCase(second, "select(.id == \"second\")", List(second).right)
    )
  }

  val classifierTestInput =
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False)

  val numbers = PreludeTestCase(classifierTestInput, ".[] | numbers", List(JSON.Num(1)).right)

  val arrays = PreludeTestCase(classifierTestInput, ".[] | arrays", List(JSON.Arr()).right)

  val objects = PreludeTestCase(classifierTestInput, ".[] | objects", List(JSON.Obj()).right)

  val iterables = PreludeTestCase(classifierTestInput, ".[] | iterables", List(JSON.Arr(), JSON.Obj()).right)

  val booleans = PreludeTestCase(classifierTestInput, ".[] | booleans", List(JSON.True, JSON.False).right)

  val strings = PreludeTestCase(classifierTestInput, ".[] | strings", List(JSON.Str("foo")).right)

  val nulls = PreludeTestCase(classifierTestInput, ".[] | nulls", List(JSON.Null).right)

  val values = PreludeTestCase(
    classifierTestInput,
    ".[] | values",
    List(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.True, JSON.False).right
  )

  val scalars = PreludeTestCase(
    classifierTestInput,
    ".[] | scalars",
    List(JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False).right
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
