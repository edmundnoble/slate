package qq

import org.scalatest.Assertion
import upickle.Js

import scala.concurrent.Future
import scalaz.\/
import scalaz.syntax.either._
import scalaz.syntax.std.`try`._
import monix.execution.Scheduler
import AsyncTestUtil._

case class RunnerTest(input: Js.Value, program: String, expectedOutputOrException: Exception \/ List[Js.Value])

object RunnerTest {

  import org.scalatest.Matchers._

  def runTest[AnyTy](runtime: QQRuntime[AnyTy], fromUpickle: Js.Value => AnyTy, toUpickle: AnyTy => Js.Value, test: RunnerTest)
                    (implicit sch: Scheduler): Future[Assertion] =
    Runner
      .run(runtime, test.program)(List(fromUpickle(test.input)))
      .materialize
      .map { outputOrExceptionTry =>
        val outputUpickleOrExceptionTry = outputOrExceptionTry.map(_.map(toUpickle))
        outputUpickleOrExceptionTry.toDisjunction should be(test.expectedOutputOrException)
      }
      .runAsync

  val identityProgram = {
    val dict = Js.Obj("1" -> Js.Num(2), "3" -> Js.Num(4))
    RunnerTest(dict, ".", List(dict).right)
  }

  val selectKeyProgram = RunnerTest(
    Js.Obj("lol" -> Js.Str("test")),
    ".lol",
    List(Js.Str("test")).right
  )

  val ensequencedFilters = RunnerTest(
    Js.Obj("lol" -> Js.Str("lol1"), "wat" -> Js.Str("wat1")),
    ".lol, .wat",
    List(Js.Str("lol1"), Js.Str("wat1")).right
  )

  val enlistedFilters = RunnerTest(
    Js.Obj("lol" -> Js.Str("lol1"), "wat" -> Js.Str("wat1")),
    "[.lol, .wat]",
    List(Js.Arr(Js.Str("lol1"), Js.Str("wat1"))).right
  )

  val collectResults = RunnerTest(
    Js.Obj("titles" -> Js.Arr(Js.Str("lol1"), Js.Str("wat1"))),
    ".titles[]",
    List(Js.Str("lol1"), Js.Str("wat1")).right)

  val enjectedFilters = RunnerTest(
    Js.Obj(
      "user" -> Js.Str("stedolan"),
      "titleName" -> Js.Arr(Js.Str("title1"), Js.Str("title2")),
      "titles" -> Js.Arr(Js.Str("JQ Primer"), Js.Str("More JQ"))
    ),
    "{user, (.titleName[]): .titles[]}",
    List(
      Js.Obj(
        "title1" -> Js.Str("JQ Primer"),
        "user" -> Js.Str("stedolan")
      ),
      Js.Obj(
        "title1" -> Js.Str("More JQ"),
        "user" -> Js.Str("stedolan")
      ),
      Js.Obj(
        "title2" -> Js.Str("JQ Primer"),
        "user" -> Js.Str("stedolan")
      ),
      Js.Obj(
        "title2" -> Js.Str("More JQ"),
        "user" -> Js.Str("stedolan")
      )
    ).right)

  val pipes = RunnerTest(
    Js.Arr(Js.Obj("name" -> Js.Str("JSON"), "good" -> Js.True), Js.Obj("name" -> Js.Str("XML"), "good" -> Js.False)),
    ".[] | .name",
    List(Js.Str("JSON"), Js.Str("XML")).right
  )

  val lengthTest = RunnerTest(
    Js.Arr(Js.Arr(Js.Num(1), Js.Num(2)), Js.Str("string"), Js.Obj("a" -> Js.Num(2)), Js.Null),
    ".[] | length",
    List(Js.Num(2), Js.Num(6), Js.Num(1), Js.Num(0)).right
  )

  val keys = RunnerTest(
    Js.Obj("abc" -> Js.Num(1), "abcd" -> Js.Num(2), "Foo" -> Js.Num(3)),
    "keys",
    List(Js.Arr(Js.Str("abc"), Js.Str("abcd"), Js.Str("Foo"))).right
  )

  val numbers = RunnerTest(
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    ".[] | numbers",
    List(Js.Num(1)).right
  )

  val arrays = RunnerTest(
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    ".[] | arrays",
    List(Js.Arr()).right
  )

  val objects = RunnerTest(
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    ".[] | objects",
    List(Js.Obj()).right
  )

  val iterables = RunnerTest(
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    ".[] | iterables",
    List(Js.Arr(), Js.Obj()).right
  )

  val booleans = RunnerTest(
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    ".[] | booleans",
    List(Js.True, Js.False).right
  )

  val strings = RunnerTest(
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    ".[] | strings",
    List(Js.Str("foo")).right
  )

  val nulls = RunnerTest(
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    ".[] | nulls",
    List(Js.Null).right
  )

  val values = RunnerTest(
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    ".[] | values",
    List(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.True, Js.False).right
  )

  val scalars = RunnerTest(
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    ".[] | scalars",
    List(Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False).right
  )

  val add = RunnerTest(
    Js.Arr(Js.Num(1), Js.Arr(Js.Num(1)), Js.Str("test")),
    ".[] | (. + .)",
    List(Js.Num(2), Js.Arr(Js.Num(1), Js.Num(1)), Js.Str("testtest")).right
  )

  val multiply = RunnerTest(
    Js.Obj("nested" -> Js.Obj("nested" -> Js.Obj("nested" -> Js.Obj("nested" -> Js.Num(1))))),
    ".[] | (. * .nested)",
    List(Js.Obj("nested" -> Js.Obj("nested" -> Js.Num(1)))).right
  )

  val maths = RunnerTest(
    Js.Num(4),
    ". % (. / 2)",
    List(Js.Num(0)).right
  )

  val bedmas = RunnerTest(
    Js.Num(5),
    ". + . * .",
    List(Js.Num(30)).right
  )

  val map = RunnerTest(
    Js.Arr(Js.Num(1)),
    "def f: . + 2; map(f)",
    List(Js.Num(3)).right
  )

  val addNullException = RunnerTest(
    Js.Arr(),
    ".[0] + .[0]",
    QQRuntimeException("can't add null and null").left
  )

  val silencedException = RunnerTest(
    Js.Arr(),
    "(.[0] + .[0])?",
    List().right
  )

  val emptyObjectProgram = RunnerTest(
    Js.Null,
    "{}",
    List(Js.Obj()).right
  )


}
