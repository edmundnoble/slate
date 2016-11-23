package qq

import cats.implicits._
import org.scalactic.NormMethods._
import org.scalatest.Assertion
import qq.cc.{Runner, RuntimeErrs}
import qq.data.JSON

import scala.concurrent.Future

case class PreludeTestCase(input: JSON, program: String, expectedOutputOrException: Either[RuntimeErrs, Vector[JSON]])

class PreludeTest extends QQAsyncTestSuite {

  def runTest(test: PreludeTestCase): Future[Assertion] =
    Runner
      .run(test.program)(test.input)
      .value
      .map(_.map(_.map(_.norm)) should be(test.expectedOutputOrException.map(_.map(_.norm))))
      .runAsync

  val selectTests: Vector[PreludeTestCase] = {
    val first: JSON =
      JSON.Obj("id" -> JSON.Str("first"), "val" -> JSON.Num(1))
    val second: JSON =
      JSON.Obj("id" -> JSON.Str("second"), "val" -> JSON.Num(2))
    Vector(
      PreludeTestCase(first, "select(.id == \"second\")", Either.right(Vector.empty)),
      PreludeTestCase(second, "select(.id == \"second\")", Either.right(Vector(second)))
    )
  }

  val classifierTestInput =
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False)

  val numbers = PreludeTestCase(classifierTestInput, ".[] | numbers", Either.right(Vector(JSON.Num(1))))

  val arrays = PreludeTestCase(classifierTestInput, ".[] | arrays", Either.right(Vector(JSON.Arr())))

  val objects = PreludeTestCase(classifierTestInput, ".[] | objects", Either.right(Vector(JSON.Obj())))

  val iterables = PreludeTestCase(classifierTestInput, ".[] | iterables", Either.right(Vector(JSON.Arr(), JSON.Obj())))

  val booleans = PreludeTestCase(classifierTestInput, ".[] | booleans", Either.right(Vector(JSON.True, JSON.False)))

  val strings = PreludeTestCase(classifierTestInput, ".[] | strings", Either.right(Vector(JSON.Str("foo"))))

  val nulls = PreludeTestCase(classifierTestInput, ".[] | nulls", Either.right(Vector(JSON.Null)))

  val values = PreludeTestCase(
    classifierTestInput,
    ".[] | values",
    Either.right(Vector(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.True, JSON.False))
  )

  val scalars = PreludeTestCase(
    classifierTestInput,
    ".[] | scalars",
    Either.right(Vector(JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False))
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
