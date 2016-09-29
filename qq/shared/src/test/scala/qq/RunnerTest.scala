package qq

import monix.execution.Scheduler
import org.scalatest.Assertion
import qq.cc.{QQRuntime, QQRuntimeException, Runner}
import qq.data.JSON

import scala.concurrent.Future
import scalaz.\/
import scalaz.syntax.either._
import scalaz.syntax.std.`try`._
import org.scalactic.NormMethods._

case class RunnerTest(input: JSON, program: String, expectedOutputOrException: Exception \/ List[JSON])

object RunnerTest {

  import CompilerTest.canonicalized
  import org.scalatest.Matchers._

  def runTest[J](runtime: QQRuntime[J], fromJSON: JSON => J, toJSON: J => JSON, test: RunnerTest)
                (implicit sch: Scheduler): Future[Assertion] =
    Runner
      .run(runtime, test.program)(List(fromJSON(test.input)))
      .materialize
      .map { outputOrExceptionTry =>
        val outputJSONOrExceptionTry = outputOrExceptionTry.map(_.map(toJSON))
        outputJSONOrExceptionTry.map(_.map(_.norm)).toDisjunction should be(test.expectedOutputOrException.map(_.map(_.norm)))
      }
      .runAsync

  val identityProgram = {
    val dict = JSON.ObjList("1" -> JSON.Num(2), "3" -> JSON.Num(4))
    RunnerTest(dict, ".", List(dict).right)
  }

  val selectKeyProgram = RunnerTest(
    JSON.ObjList("lol" -> JSON.Str("test")),
    ".lol",
    List(JSON.Str("test")).right
  )

  val ensequencedFilters = RunnerTest(
    JSON.ObjList("lol" -> JSON.Str("lol1"), "wat" -> JSON.Str("wat1")),
    ".lol, .wat",
    List(JSON.Str("lol1"), JSON.Str("wat1")).right
  )

  val enlistedFilters = RunnerTest(
    JSON.ObjList("lol" -> JSON.Str("lol1"), "wat" -> JSON.Str("wat1")),
    "[.lol, .wat]",
    List(JSON.Arr(JSON.Str("lol1"), JSON.Str("wat1"))).right
  )

  val collectResults = List(
    RunnerTest(
      JSON.ObjList("titles" -> JSON.Arr(JSON.Str("lol1"), JSON.Str("wat1"))),
      ".titles[]",
      List(JSON.Str("lol1"), JSON.Str("wat1")).right
    ),
    RunnerTest(
      JSON.Num(1),
      ".[]",
      QQRuntimeException("Tried to flatten 1.000000 but it's not an array").left
    )
  )

  val enjectedFilters = RunnerTest(
    JSON.ObjList(
      "user" -> JSON.Str("stedolan"),
      "titleName" -> JSON.Arr(JSON.Str("title1"), JSON.Str("title2")),
      "titles" -> JSON.Arr(JSON.Str("JQ Primer"), JSON.Str("More JQ"))
    ),
    "{user, (.titleName[]): .titles[]}",
    List(
      JSON.ObjList(
        "title1" -> JSON.Str("JQ Primer"),
        "user" -> JSON.Str("stedolan")
      ),
      JSON.ObjList(
        "title1" -> JSON.Str("More JQ"),
        "user" -> JSON.Str("stedolan")
      ),
      JSON.ObjList(
        "title2" -> JSON.Str("JQ Primer"),
        "user" -> JSON.Str("stedolan")
      ),
      JSON.ObjList(
        "title2" -> JSON.Str("More JQ"),
        "user" -> JSON.Str("stedolan")
      )
    ).right)

  val pipes = RunnerTest(
    JSON.Arr(JSON.ObjList("name" -> JSON.Str("JSON"), "good" -> JSON.True), JSON.ObjList("name" -> JSON.Str("XML"), "good" -> JSON.False)),
    ".[] | .name",
    List(JSON.Str("JSON"), JSON.Str("XML")).right
  )

  val lengthTest = RunnerTest(
    JSON.Arr(JSON.Arr(JSON.Num(1), JSON.Num(2)), JSON.Str("string"), JSON.ObjList("a" -> JSON.Num(2)), JSON.Null),
    ".[] | length",
    List(JSON.Num(2), JSON.Num(6), JSON.Num(1), JSON.Num(0)).right
  )

  val keys = RunnerTest(
    JSON.ObjList("abc" -> JSON.Num(1), "abcd" -> JSON.Num(2), "Foo" -> JSON.Num(3)),
    "keys",
    List(JSON.Arr(JSON.Str("abc"), JSON.Str("abcd"), JSON.Str("Foo"))).right
  )

  val numbers = RunnerTest(
    JSON.Arr(JSON.Arr(), JSON.ObjList(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | numbers",
    List(JSON.Num(1)).right
  )

  val arrays = RunnerTest(
    JSON.Arr(JSON.Arr(), JSON.ObjList(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | arrays",
    List(JSON.Arr()).right
  )

  val objects = RunnerTest(
    JSON.Arr(JSON.Arr(), JSON.ObjList(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | objects",
    List(JSON.ObjList()).right
  )

  val iterables = RunnerTest(
    JSON.Arr(JSON.Arr(), JSON.ObjList(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | iterables",
    List(JSON.Arr(), JSON.ObjList()).right
  )

  val booleans = RunnerTest(
    JSON.Arr(JSON.Arr(), JSON.ObjList(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | booleans",
    List(JSON.True, JSON.False).right
  )

  val strings = RunnerTest(
    JSON.Arr(JSON.Arr(), JSON.ObjList(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | strings",
    List(JSON.Str("foo")).right
  )

  val nulls = RunnerTest(
    JSON.Arr(JSON.Arr(), JSON.ObjList(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | nulls",
    List(JSON.Null).right
  )

  val values = RunnerTest(
    JSON.Arr(JSON.Arr(), JSON.ObjList(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | values",
    List(JSON.Arr(), JSON.ObjList(), JSON.Num(1), JSON.Str("foo"), JSON.True, JSON.False).right
  )

  val scalars = RunnerTest(
    JSON.Arr(JSON.Arr(), JSON.ObjList(), JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False),
    ".[] | scalars",
    List(JSON.Num(1), JSON.Str("foo"), JSON.Null, JSON.True, JSON.False).right
  )

  val add = RunnerTest(
    JSON.Arr(JSON.Num(1), JSON.Arr(JSON.Num(1)), JSON.Str("test")),
    ".[] | (. + .)",
    List(JSON.Num(2), JSON.Arr(JSON.Num(1), JSON.Num(1)), JSON.Str("testtest")).right
  )

  val multiply = RunnerTest(
    JSON.ObjList("nested" -> JSON.ObjList("nested" -> JSON.ObjList("nested" -> JSON.ObjList("nested" -> JSON.Num(1))))),
    ".[] | (. * .nested)",
    List(JSON.ObjList("nested" -> JSON.ObjList("nested" -> JSON.Num(1)))).right
  )

  val maths = RunnerTest(
    JSON.Num(4),
    ". % (. / 2)",
    List(JSON.Num(0)).right
  )

  val bedmas = RunnerTest(
    JSON.Num(5),
    ". + . * .",
    List(JSON.Num(30)).right
  )

  val map = RunnerTest(
    JSON.Arr(JSON.Num(1)),
    "def f: . + 2; map(f)",
    List(JSON.Num(3)).right
  )

  val addNullException = RunnerTest(
    JSON.Arr(),
    ".[0] + .[0]",
    QQRuntimeException("can't add null and null").left
  )

  val silencedException = RunnerTest(
    JSON.Arr(),
    "(.[0] + .[0])?",
    List().right
  )

  val emptyObjectProgram = RunnerTest(
    JSON.Null,
    "{}",
    List(JSON.ObjList()).right
  )

  val constants = RunnerTest(
    JSON.Null,
    "true, false, null",
    List(JSON.True, JSON.False, JSON.Null).right
  )

  val base64Encode = RunnerTest(
    JSON.Str("hi"),
    "b64Encode",
    List(JSON.Str("aGk=")).right
  )


}
