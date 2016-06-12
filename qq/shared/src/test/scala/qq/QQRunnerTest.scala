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
    Js.Obj("user" -> Js.Str("stedolan"), "titleName" -> Js.Arr(Js.Str("title1"), Js.Str("title2")), "titles" -> Js.Arr(Js.Str("JQ Primer"), Js.Str("More JQ"))),
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
}
