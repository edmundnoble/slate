package qq

import cats.data.NonEmptyList
import cats.implicits._
import org.scalactic.NormMethods._
import org.scalatest.Assertion
import qq.cc.QQRuntimeException._
import qq.cc.{QQRuntimeError, Runner, RuntimeErrs, TypeError}
import qq.data.JSON

import scala.concurrent.Future

case class RunnerTestCase(input: JSON, program: String, expectedOutputOrException: Either[RuntimeErrs, List[JSON]])

class RunnerTest extends QQAsyncTestSuite {

  def runTest(test: RunnerTestCase): Future[Assertion] =
    Runner
      .run(test.program)(test.input)
      .value
      .map(_.map(_.map(_.norm)) should be(test.expectedOutputOrException.map(_.map(_.norm))))
      .runAsync

  val identityProgram: RunnerTestCase = {
    val dict = JSON.Obj("1" -> JSON.Num(2), "3" -> JSON.Num(4))
    RunnerTestCase(dict, ".", Either.right(List(dict)))
  }

  val selectKeyProgram = RunnerTestCase(
    JSON.Obj("lol" -> JSON.Str("test")),
    ".lol",
    Either.right(List(JSON.Str("test")))
  )

  val ensequencedFilters = RunnerTestCase(
    JSON.Obj("lol" -> JSON.Str("lol1"), "wat" -> JSON.Str("wat1")),
    ".lol, .wat",
    Either.right(List(JSON.Str("lol1"), JSON.Str("wat1")))
  )

  val enlistedFilters = RunnerTestCase(
    JSON.Obj("lol" -> JSON.Str("lol1"), "wat" -> JSON.Str("wat1")),
    "[.lol, .wat]",
    Either.right(List(JSON.Arr(JSON.Str("lol1"), JSON.Str("wat1"))))
  )

  val collectResults = List(
    RunnerTestCase(
      JSON.Obj("titles" -> JSON.Arr(JSON.Str("lol1"), JSON.Str("wat1"))),
      ".titles[]",
      Either.right(List(JSON.Str("lol1"), JSON.Str("wat1")))
    ),
    RunnerTestCase(JSON.Num(1), ".[]", typeError("flatten", "array" -> JSON.Num(1)))
  )

  val enjectedFilters = RunnerTestCase(
    JSON.Obj(
      "user" -> JSON.Str("stedolan"),
      "titleName" -> JSON.Arr(JSON.Str("title1"), JSON.Str("title2")),
      "titles" -> JSON.Arr(JSON.Str("JQ Primer"), JSON.Str("More JQ"))
    ),
    "{user, (.titleName[]): .titles[]}",
    Either.right(List(
      JSON.Obj(
        "title1" -> JSON.Str("JQ Primer"),
        "user" -> JSON.Str("stedolan")
      ),
      JSON.Obj(
        "title1" -> JSON.Str("More JQ"),
        "user" -> JSON.Str("stedolan")
      ),
      JSON.Obj(
        "title2" -> JSON.Str("JQ Primer"),
        "user" -> JSON.Str("stedolan")
      ),
      JSON.Obj(
        "title2" -> JSON.Str("More JQ"),
        "user" -> JSON.Str("stedolan")
      )
    )))

  val pipes = RunnerTestCase(
    JSON.Arr(JSON.Obj("name" -> JSON.Str("JSON"), "good" -> JSON.True), JSON.Obj("name" -> JSON.Str("XML"), "good" -> JSON.False)),
    ".[] | .name",
    Either.right(List(JSON.Str("JSON"), JSON.Str("XML")))
  )

  val lengthTest = RunnerTestCase(
    JSON.Arr(JSON.Arr(JSON.Num(1), JSON.Num(2)), JSON.Str("string"), JSON.Obj("a" -> JSON.Num(2)), JSON.Null),
    ".[] | length",
    Either.right(List(JSON.Num(2), JSON.Num(6), JSON.Num(1), JSON.Num(0)))
  )

  val keys = RunnerTestCase(
    JSON.Obj("abc" -> JSON.Num(1), "abcd" -> JSON.Num(2), "Foo" -> JSON.Num(3)),
    "keys",
    Either.right(List(JSON.Arr(JSON.Str("abc"), JSON.Str("abcd"), JSON.Str("Foo"))))
  )

  val add = RunnerTestCase(
    JSON.Arr(JSON.Num(1), JSON.Arr(JSON.Num(1)), JSON.Str("test")),
    ".[] | (. + .)",
    Either.right(List(JSON.Num(2), JSON.Arr(JSON.Num(1), JSON.Num(1)), JSON.Str("testtest")))
  )

  val multiply = RunnerTestCase(
    JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Num(1))))),
    ".[] | (. * .nested)",
    Either.right(List(JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Num(1)))))
  )

  val maths =
    RunnerTestCase(JSON.Num(4), ". % (. / 2)", Either.right(List(JSON.Num(0))))

  val bedmas =
    RunnerTestCase(JSON.Num(5), ". + . * .", Either.right(List(JSON.Num(30))))

  val map =
    RunnerTestCase(JSON.Arr(JSON.Num(1)), "def f: . + 2; map(f)", Either.right(List(JSON.Num(3))))

  val addNullException =
    RunnerTestCase(JSON.Arr(), ".[0] + .[0]", typeError(
      "add",
      "number | string | array | object" -> JSON.Null,
      "number | string | array | object" -> JSON.Null
    ))

  val silencedException =
    RunnerTestCase(JSON.Arr(), "(.[0] + .[0])?", Either.right(List()))

  val emptyObjectProgram =
    RunnerTestCase(JSON.Null, "{}", Either.right(List(JSON.Obj())))

  val constants =
    RunnerTestCase(JSON.Null, "true, false, null", Either.right(List(JSON.True, JSON.False, JSON.Null)))

  val base64Encode =
    RunnerTestCase(JSON.Str("hi"), "b64Encode", Either.right(List(JSON.Str("aGk="))))

  val multipleErrorTests = List(
    RunnerTestCase(JSON.Obj("a" -> JSON.Arr(JSON.Obj("b" -> JSON.Null), JSON.Obj("b" -> JSON.Null))),
      ".a.[].b.c",
      Either.left(
        NonEmptyList.of[QQRuntimeError](
          TypeError("select key c", "object" -> JSON.Null),
          TypeError("select key c", "object" -> JSON.Null)
        )
      )
    )
  )

  "identity" in runTest(identityProgram)
  "ensequenced filters" in runTest(ensequencedFilters)
  "enlisted filter" in runTest(enlistedFilters)
  "select key" in runTest(selectKeyProgram)
  "collect results" in collectResults.traverse(runTest)
  "enject filter" in runTest(enjectedFilters)
  "pipes" in runTest(pipes)
  "length" in runTest(lengthTest)
  "keys" in runTest(keys)
  "add" in runTest(add)
  "maths" in runTest(maths)
  "bedmas" in runTest(bedmas)
  "map" in runTest(map)
  "multiply" in runTest(multiply)
  "add null exception" in runTest(addNullException)
  "silenced exception" in runTest(silencedException)
  "empty object" in runTest(emptyObjectProgram)
  "constants" in runTest(constants)
  "base64 encode" in runTest(base64Encode)
  "multiple errors" in multipleErrorTests.traverse(runTest)

}
