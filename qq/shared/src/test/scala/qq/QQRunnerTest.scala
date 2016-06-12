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
    "{user, title: .titles[]}",
    Js.Obj("user" -> Js.Str("stedolan"), "titles" -> Js.Arr(Js.Str("JQ Primer"), Js.Str("More JQ"))),
    List(
      Js.Obj(
        "user" -> Js.Str("stedolan"),
        "title" -> Js.Str("JQ Primer")
      ),
      Js.Obj(
        "user" -> Js.Str("stedolan"),
        "title" -> Js.Str("More JQ")
      )
    ))
}
