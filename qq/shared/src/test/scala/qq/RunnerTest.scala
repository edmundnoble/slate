package qq

import org.scalatest.Assertion
import qq.cc.{JSONRuntime, QQRuntimeException, Runner}
import qq.data.JSON

import scala.concurrent.Future
import scalaz.\/
import scalaz.syntax.either._
import scalaz.syntax.std.`try`._
import org.scalactic.NormMethods._
import scalaz.syntax.traverse._
import scalaz.std.scalaFuture._
import scalaz.std.list._

case class RunnerTestCase(input: JSON, program: String, expectedOutputOrException: Exception \/ List[JSON])

class RunnerTest extends QQAsyncTestSuite {

  def runTest(test: RunnerTestCase): Future[Assertion] =
    Runner
      .run(JSONRuntime, test.program)(List(test.input))
      .materialize
      .map(_.map(_.map(_.norm)).toDisjunction should be(test.expectedOutputOrException.map(_.map(_.norm))))
      .runAsync

  val identityProgram: RunnerTestCase = {
    val dict = JSON.Obj("1" -> JSON.Num(2), "3" -> JSON.Num(4))
    RunnerTestCase(dict, ".", List(dict).right)
  }

  val selectKeyProgram = RunnerTestCase(
    JSON.Obj("lol" -> JSON.Str("test")),
    ".lol",
    List(JSON.Str("test")).right
  )

  val ensequencedFilters = RunnerTestCase(
    JSON.Obj("lol" -> JSON.Str("lol1"), "wat" -> JSON.Str("wat1")),
    ".lol, .wat",
    List(JSON.Str("lol1"), JSON.Str("wat1")).right
  )

  val enlistedFilters = RunnerTestCase(
    JSON.Obj("lol" -> JSON.Str("lol1"), "wat" -> JSON.Str("wat1")),
    "[.lol, .wat]",
    List(JSON.Arr(JSON.Str("lol1"), JSON.Str("wat1"))).right
  )

  val collectResults = List(
    RunnerTestCase(
      JSON.Obj("titles" -> JSON.Arr(JSON.Str("lol1"), JSON.Str("wat1"))),
      ".titles[]",
      List(JSON.Str("lol1"), JSON.Str("wat1")).right
    ),
    RunnerTestCase(
      JSON.Num(1),
      ".[]",
      QQRuntimeException("Tried to flatten 1.000000 but it's not an array").left
    )
  )

  val enjectedFilters = RunnerTestCase(
    JSON.Obj(
      "user" -> JSON.Str("stedolan"),
      "titleName" -> JSON.Arr(JSON.Str("title1"), JSON.Str("title2")),
      "titles" -> JSON.Arr(JSON.Str("JQ Primer"), JSON.Str("More JQ"))
    ),
    "{user, (.titleName[]): .titles[]}",
    List(
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
    ).right)

  val pipes = RunnerTestCase(
    JSON.Arr(JSON.Obj("name" -> JSON.Str("JSON"), "good" -> JSON.True), JSON.Obj("name" -> JSON.Str("XML"), "good" -> JSON.False)),
    ".[] | .name",
    List(JSON.Str("JSON"), JSON.Str("XML")).right
  )

  val lengthTest = RunnerTestCase(
    JSON.Arr(JSON.Arr(JSON.Num(1), JSON.Num(2)), JSON.Str("string"), JSON.Obj("a" -> JSON.Num(2)), JSON.Null),
    ".[] | length",
    List(JSON.Num(2), JSON.Num(6), JSON.Num(1), JSON.Num(0)).right
  )

  val keys = RunnerTestCase(
    JSON.Obj("abc" -> JSON.Num(1), "abcd" -> JSON.Num(2), "Foo" -> JSON.Num(3)),
    "keys",
    List(JSON.Arr(JSON.Str("abc"), JSON.Str("abcd"), JSON.Str("Foo"))).right
  )

  val numbers = RunnerTestCase(
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | numbers",
    List(JSON.Num(1)).right
  )

  val arrays = RunnerTestCase(
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | arrays",
    List(JSON.Arr()).right
  )

  val objects = RunnerTestCase(
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | objects",
    List(JSON.Obj()).right
  )

  val iterables = RunnerTestCase(
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | iterables",
    List(JSON.Arr(), JSON.Obj()).right
  )

  val booleans = RunnerTestCase(
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | booleans",
    List(JSON.True, JSON.False).right
  )

  val strings = RunnerTestCase(
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | strings",
    List(JSON.Str("foo")).right
  )

  val nulls = RunnerTestCase(
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | nulls",
    List(JSON.Null).right
  )

  val values = RunnerTestCase(
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | values",
    List(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.True, JSON.False).right
  )

  val scalars = RunnerTestCase(
    JSON.Arr(JSON.Arr(), JSON.Obj(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | scalars",
    List(JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False).right
  )

  val add = RunnerTestCase(
    JSON.Arr(JSON.Num(1), JSON.Arr(JSON.Num(1)), JSON.Str("test")),
    ".[] | (. + .)",
    List(JSON.Num(2), JSON.Arr(JSON.Num(1), JSON.Num(1)), JSON.Str("testtest")).right
  )

  val multiply = RunnerTestCase(
    JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Num(1))))),
    ".[] | (. * .nested)",
    List(JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Num(1)))).right
  )

  val maths = RunnerTestCase(
    JSON.Num(4),
    ". % (. / 2)",
    List(JSON.Num(0)).right
  )

  val bedmas = RunnerTestCase(
    JSON.Num(5),
    ". + . * .",
    List(JSON.Num(30)).right
  )

  val map = RunnerTestCase(
    JSON.Arr(JSON.Num(1)),
    "def f: . + 2; map(f)",
    List(JSON.Num(3)).right
  )

  val addNullException = RunnerTestCase(
    JSON.Arr(),
    ".[0] + .[0]",
    QQRuntimeException("can't add null and null").left
  )

  val silencedException = RunnerTestCase(
    JSON.Arr(),
    "(.[0] + .[0])?",
    List().right
  )

  val emptyObjectProgram = RunnerTestCase(
    JSON.Null,
    "{}",
    List(JSON.Obj()).right
  )

  val constants = RunnerTestCase(
    JSON.Null,
    "true, false, null",
    List(JSON.True, JSON.False, JSON.Null).right
  )

  val base64Encode = RunnerTestCase(
    JSON.Str("hi"),
    "b64Encode",
    List(JSON.Str("aGk=")).right
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

}
