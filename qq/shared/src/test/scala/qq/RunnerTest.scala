package qq

import org.scalactic.NormMethods._
import org.scalatest.Assertion
import qq.cc.{QQRuntimeError, QQRuntimeException, Runner, TypeError}
import qq.data.JSON

import scala.concurrent.Future
import scalaz.{ValidationNel, \/}
import scalaz.std.list._
import scalaz.std.scalaFuture._
import scalaz.syntax.validation._
import scalaz.syntax.std.`try`._
import scalaz.syntax.traverse._
import QQRuntimeException._

case class RunnerTestCase(input: JSON, program: String, expectedOutputOrException: ValidationNel[QQRuntimeError, List[JSON]])

class RunnerTest extends QQAsyncTestSuite {

  def runTest(test: RunnerTestCase): Future[Assertion] =
    Runner
      .run(test.program)(List(test.input))
      .value
      .map(_.map(_.map(_.norm)) should be(test.expectedOutputOrException.map(_.map(_.norm))))
      .runAsync

  val identityProgram: RunnerTestCase = {
    val dict = JSON.Obj("1" -> JSON.Num(2), "3" -> JSON.Num(4))
    RunnerTestCase(dict, ".", List(dict).successNel)
  }

  val selectKeyProgram = RunnerTestCase(
    JSON.Obj("lol" -> JSON.Str("test")),
    ".lol",
    List(JSON.Str("test")).successNel
  )

  val ensequencedFilters = RunnerTestCase(
    JSON.Obj("lol" -> JSON.Str("lol1"), "wat" -> JSON.Str("wat1")),
    ".lol, .wat",
    List(JSON.Str("lol1"), JSON.Str("wat1")).successNel
  )

  val enlistedFilters = RunnerTestCase(
    JSON.Obj("lol" -> JSON.Str("lol1"), "wat" -> JSON.Str("wat1")),
    "[.lol, .wat]",
    List(JSON.Arr(JSON.Str("lol1"), JSON.Str("wat1"))).successNel
  )

  val collectResults = List(
    RunnerTestCase(
      JSON.Obj("titles" -> JSON.Arr(JSON.Str("lol1"), JSON.Str("wat1"))),
      ".titles[]",
      List(JSON.Str("lol1"), JSON.Str("wat1")).successNel
    ),
    RunnerTestCase(JSON.Num(1), ".[]", typeError("flatten", "array" -> JSON.Num(1)).failureNel)
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
    ).successNel)

  val pipes = RunnerTestCase(
    JSON.Arr(JSON.Obj("name" -> JSON.Str("JSON"), "good" -> JSON.True), JSON.Obj("name" -> JSON.Str("XML"), "good" -> JSON.False)),
    ".[] | .name",
    List(JSON.Str("JSON"), JSON.Str("XML")).successNel
  )

  val lengthTest = RunnerTestCase(
    JSON.Arr(JSON.Arr(JSON.Num(1), JSON.Num(2)), JSON.Str("string"), JSON.Obj("a" -> JSON.Num(2)), JSON.Null),
    ".[] | length",
    List(JSON.Num(2), JSON.Num(6), JSON.Num(1), JSON.Num(0)).successNel
  )

  val keys = RunnerTestCase(
    JSON.Obj("abc" -> JSON.Num(1), "abcd" -> JSON.Num(2), "Foo" -> JSON.Num(3)),
    "keys",
    List(JSON.Arr(JSON.Str("abc"), JSON.Str("abcd"), JSON.Str("Foo"))).successNel
  )

  val add = RunnerTestCase(
    JSON.Arr(JSON.Num(1), JSON.Arr(JSON.Num(1)), JSON.Str("test")),
    ".[] | (. + .)",
    List(JSON.Num(2), JSON.Arr(JSON.Num(1), JSON.Num(1)), JSON.Str("testtest")).successNel
  )

  val multiply = RunnerTestCase(
    JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Num(1))))),
    ".[] | (. * .nested)",
    List(JSON.Obj("nested" -> JSON.Obj("nested" -> JSON.Num(1)))).successNel
  )

  val maths =
    RunnerTestCase(JSON.Num(4), ". % (. / 2)", List(JSON.Num(0)).successNel)

  val bedmas =
    RunnerTestCase(JSON.Num(5), ". + . * .", List(JSON.Num(30)).successNel)

  val map =
    RunnerTestCase(JSON.Arr(JSON.Num(1)), "def f: . + 2; map(f)", List(JSON.Num(3)).successNel)

  val addNullException =
    RunnerTestCase(JSON.Arr(), ".[0] + .[0]", typeError(
      "add",
      "number | string | array | object" -> JSON.Null,
      "number | string | array | object" -> JSON.Null
    ).failureNel)

  val silencedException =
    RunnerTestCase(JSON.Arr(), "(.[0] + .[0])?", List().successNel)

  val emptyObjectProgram =
    RunnerTestCase(JSON.Null, "{}", List(JSON.Obj()).successNel)

  val constants =
    RunnerTestCase(JSON.Null, "true, false, null", List(JSON.True, JSON.False, JSON.Null).successNel)

  val base64Encode =
    RunnerTestCase(JSON.Str("hi"), "b64Encode", List(JSON.Str("aGk=")).successNel)

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

}
