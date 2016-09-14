package qq

import fastparse.parsers.Terminals.End

import scalaz.\/
import scalaz.std.list._

class ParserTest extends QQSyncTestSuite {

  import FilterDSL._

  val emptySized: List[String] = Nil

  "parse plain dots" in {
    Parser.dot.parse(".").get.value.shouldBe(())
  }

  "parse selections" - {
    "select key" in {
      Parser.selectKey.parse("key").get.value shouldBe selectKey("key")
      Parser.selectKey.parse("viewUrl").get.value shouldBe selectKey("viewUrl")
    }
    "select index" in {
      Parser.selectIndex.parse("1").get.value shouldBe selectIndex(1)
      Parser.selectIndex.parse("-1").get.value shouldBe selectIndex(-1)
    }
  }

  "parse dottable filters" in {
    Parser.dottableSimpleFilter.parse("[\"filter \"]").get.value shouldBe selectKey("filter ")
  }

  "parse dotted filters" in {
    Parser.dottedFilter.parse(".").get.value shouldBe id
    Parser.dottedFilter.parse(".[]").get.value shouldBe compose(id, collectResults(id))
    Parser.dottedFilter.parse(".key").get.value shouldBe compose(id, selectKey("key"))
    Parser.dottedFilter.parse(".[1]").get.value shouldBe compose(id, selectIndex(1))
    Parser.dottedFilter.parse(".[-1]").get.value shouldBe compose(id, selectIndex(-1))
    Parser.dottedFilter.parse(".[1][]").get.value shouldBe compose(id, collectResults(selectIndex(1)))
    Parser.dottedFilter.parse(".key[]").get.value shouldBe compose(id, collectResults(selectKey("key")))
    Parser.dottedFilter.parse(".key.otherkey.1.[1][].[1:3].[\"this key\"]").get.value shouldBe
      compose(compose(compose(compose(compose(compose(
        id, selectKey("key")), selectKey("otherkey")), selectKey("1")),
        collectResults(selectIndex(1))), selectRange(1, 3)), selectKey("this key"))
  }

  "parse called filters" in {
    Parser.callFilter.parse("test").get.value shouldBe call("test", Nil)
    Parser.callFilter.parse("test(.)").get.value shouldBe call("test", List(id))
    Parser.callFilter.parse("test(.;.)").get.value shouldBe call("test", List(id, id))
  }

  "parse piped filters" in {
    Parser.filter.parse(".key | .dang").get.value shouldBe
      compose(compose(id, selectKey("key")), compose(id, selectKey("dang")))
    Parser.filter.parse("(.key) | (.dang)").get.value shouldBe
      compose(compose(id, selectKey("key")), compose(id, selectKey("dang")))
    Parser.filter.parse("(.key) | (dang)").get.value shouldBe
      compose(compose(id, selectKey("key")), call("dang"))
  }

  "parse ensequenced filters" in {
    Parser.filter.parse(".key, .dang").get.value shouldBe
      ensequence(compose(id, selectKey("key")), compose(id, selectKey("dang"))
      )
  }

  "parse enlisted filters" in {
    Parser.enlistedFilter.parse("[.key, .dang]").get.value shouldBe
      enlist(ensequence(compose(id, selectKey("key")), compose(id, selectKey("dang")))
      )
  }

  "parse definitions" in {
    Parser.definition.parse("def id: .;").get.value shouldBe
      Definition("id", emptySized, id)
  }

  "parse constants" - {
    "integers" in {
      Parser.filter.parse("1").get.value shouldBe constNumber(1)
      Parser.filter.parse("4").get.value shouldBe constNumber(4)
    }
    "strings" in {
      Parser.filter.parse(""""hello"""").get.value shouldBe constString("hello")
    }
  }

  "parse math operators" in {
    Parser.filter.parse(". + .").get.value shouldBe add(id, id)
    Parser.filter.parse(". - .").get.value shouldBe subtract(id, id)
    Parser.filter.parse(". * .").get.value shouldBe multiply(id, id)
    Parser.filter.parse(". / .").get.value shouldBe divide(id, id)
    Parser.filter.parse(". % .").get.value shouldBe modulo(id, id)
  }

  "parse enjected pairs" - {
    "string key" in (Parser.enjectPair.parse("hello: id").get.value shouldBe \/.left("hello") -> call("id"))
    "filter key" in (Parser.enjectPair.parse("(hello): id").get.value shouldBe \/.right(call("hello")) -> call("id"))
    "sugar" in (Parser.enjectPair.parse("user").get.value shouldBe \/.left("user") -> selectKey("user"))
  }

  "parse enjected filters" in {
    Parser.enjectedFilter.parse("{ sugar, user: \"user\", title: .titles[] }").get.value shouldBe
      enject(
        List(
          \/.left("sugar") -> selectKey("sugar"),
          \/.left("user") -> constString("user"),
          \/.left("title") -> compose(id, collectResults(selectKey("titles")))
        )
      )
  }

  "parse full programs" - {
    "with just a body" in (Parser.program.parse("id").get.value shouldBe
      Program[Filter](Vector.empty[Definition[Filter]].toDefinitions, call("id")))
    "with small definitions" in (Parser.program.parse("def id: .; id").get.value shouldBe
      Program[Filter](Vector(Definition[Filter]("id", emptySized, id)).toDefinitions, call("id")))
    "with whitespace at the start" in (Parser.program.parse(" .").get.value shouldBe
      Program[Filter](Vector.empty[Definition[Filter]].toDefinitions, id))
    "with whitespace at the end" in (Parser.program.parse(". ").get.value shouldBe
      Program[Filter](Vector.empty[Definition[Filter]].toDefinitions, id))
  }

  "parse string literals" in {
    Parser.escapedStringLiteral.parse(""""hello"""").get.value shouldBe "hello"
  }

  "parse string literal filters" in {
    Parser.filter.parse(""""hello"""").get.value shouldBe constString("hello")
    Parser.filter.parse(""""\\"""").get.value shouldBe constString("\\")
  }

  "parse enjected filter regression" in {
    Parser.program.parse("{user, (.titleName[]): .titles[]}").get.value
    // should just succeed
  }

  "precedence" in {
    Parser.filter.parse("""., . | . + . * .""").get.value shouldBe ensequence(id, compose(id, add(id, multiply(id, id))))
    Parser.filter.parse(""". * . + . | ., .""").get.value shouldBe ensequence(compose(add(multiply(id, id), id), id), id)
  }

  "dereference variables" - {
    "literal parsing" in (Parser.dereference.parse("$hello").get.value shouldBe deref("hello"))
    "in a filter call" in (Parser.filter.parse("f($hello)").get.value shouldBe call("f", List(deref("hello"))))
  }

  "let bindings" - {
    "plain binding" in (Parser.letAsBinding.parse("let $d as . in .").get.value shouldBe letAsBinding("d", id, id))
    "in a filter" in (Parser.filter.parse(". | let $d as . in .").get.value shouldBe compose(id, letAsBinding("d", id, id)))
  }

}
