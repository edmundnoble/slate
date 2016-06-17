package qq

import upickle.Js

case class QQRunnerTest(program: String, input: Js.Value, expectedOutput: List[Js.Value])

object QQRunnerTest {
  val identityProgram = {
    val dict = Js.Obj("1" -> Js.Num(2), "3" -> Js.Num(4))
    QQRunnerTest(".", dict, List(dict))
  }
  val selectKeyProgram = QQRunnerTest(".lol",
    Js.Obj("lol" -> Js.Str("test")),
    List(Js.Str("test")))
  val ensequencedFilters = QQRunnerTest(".lol, .wat", Js.Obj("lol" -> Js.Str("lol1"), "wat" -> Js.Str("wat1")), List(Js.Str("lol1"), Js.Str("wat1")))
  val enlistedFilters = QQRunnerTest("[.lol, .wat]", Js.Obj("lol" -> Js.Str("lol1"), "wat" -> Js.Str("wat1")), List(Js.Arr(Js.Str("lol1"), Js.Str("wat1"))))
  val collectResults = QQRunnerTest(".titles[]", Js.Obj("titles" -> Js.Arr(Js.Str("lol1"), Js.Str("wat1"))), List(Js.Str("lol1"), Js.Str("wat1")))
  val enjectedFilters = QQRunnerTest(
    "{user, (.titleName[]): .titles[]}",
    Js.Obj(
      "user" -> Js.Str("stedolan"),
      "titleName" -> Js.Arr(Js.Str("title1"), Js.Str("title2")),
      "titles" -> Js.Arr(Js.Str("JQ Primer"), Js.Str("More JQ"))
    ),
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
    ))

  val pipes = QQRunnerTest(
    ".[] | .name",
    Js.Arr(Js.Obj("name" -> Js.Str("JSON"), "good" -> Js.True), Js.Obj("name" -> Js.Str("XML"), "good" -> Js.False)),
    List(Js.Str("JSON"), Js.Str("XML"))
  )

  val length = QQRunnerTest(
    ".[] | length",
    Js.Arr(Js.Arr(Js.Num(1), Js.Num(2)), Js.Str("string"), Js.Obj("a" -> Js.Num(2)), Js.Null),
    List(Js.Num(2), Js.Num(6), Js.Num(1), Js.Num(0))
  )

  val keys = QQRunnerTest(
    "keys",
    Js.Obj("abc" -> Js.Num(1), "abcd" -> Js.Num(2), "Foo" -> Js.Num(3)),
    List(Js.Arr(Js.Str("abc"), Js.Str("abcd"), Js.Str("Foo")))
  )

  val numbers = QQRunnerTest(
    ".[]|numbers",
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    List(Js.Num(1))
  )

  val arrays = QQRunnerTest(
    ".[]|arrays",
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    List(Js.Arr())
  )

  val objects = QQRunnerTest(
    ".[]|objects",
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    List(Js.Obj())
  )

  val iterables = QQRunnerTest(
    ".[]|iterables",
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    List(Js.Arr(), Js.Obj())
  )

  val booleans = QQRunnerTest(
    ".[]|booleans",
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    List(Js.True, Js.False)
  )

  val strings = QQRunnerTest(
    ".[]|strings",
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    List(Js.Str("foo"))
  )

  val nulls = QQRunnerTest(
    ".[]|nulls",
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    List(Js.Null)
  )

  val values = QQRunnerTest(
    ".[]|values",
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    List(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.True, Js.False)
  )

  val scalars = QQRunnerTest(
    ".[]|scalars",
    Js.Arr(Js.Arr(), Js.Obj(), Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False),
    List(Js.Num(1), Js.Str("foo"), Js.Null, Js.True, Js.False)
  )

  val add = QQRunnerTest(
    "(2 + 2)",
    Js.Num(2),
    List(Js.Num(4))
  )

}
