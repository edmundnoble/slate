package qq

import qq.cc.Parser
import qq.data.{Definition, FilterAST, Program}

class ParserTest extends QQSyncTestSuite {

  import qq.data.QQDSL._

  "parse plain dots" in {
    Parser.dot.parse(".").get.value.shouldBe(())
  }

  "parse selections" - {
    "select key" in {
      Parser.fullPath.parse(".key").get.value shouldBe Vector(selectKey("key"))
      Parser.fullPath.parse(".viewUrl").get.value shouldBe Vector(selectKey("viewUrl"))
    }
    "select index" in {
      Parser.selectIndex.parse("1").get.value shouldBe selectIndex(1)
      Parser.selectIndex.parse("-1").get.value shouldBe selectIndex(-1)
    }
  }

  "parse single path components" in {
    Parser.pathComponent.parse("[\"filter \"]").get.value shouldBe Vector(selectKey("filter "))
  }

  "parse full paths" in {
    Parser.fullPath.parse(".").get.value shouldBe Vector.empty
    Parser.fullPath.parse(".[]").get.value shouldBe Vector(collectResults)
    Parser.fullPath.parse(".key").get.value shouldBe Vector(selectKey("key"))
    Parser.fullPath.parse(".[1]").get.value shouldBe Vector(selectIndex(1))
    Parser.fullPath.parse(".[-1]").get.value shouldBe Vector(selectIndex(-1))
    Parser.fullPath.parse(".[1][]").get.value shouldBe Vector(selectIndex(1), collectResults)
    Parser.fullPath.parse(".key[]").get.value shouldBe Vector(selectKey("key"), collectResults)
    Parser.fullPath.parse(""".key.otherkey.1.[1][].[1:3].["this key"]""").get.value shouldBe
      Vector(selectKey("key"), selectKey("otherkey"), selectKey("1"),
        selectIndex(1), collectResults, selectRange(1, 3), selectKey("this key"))
  }

  "parse path getters" in {
    Parser.filter.parse(".key").get.value shouldBe getPathS(selectKey("key"))
  }

  "parse path setters" in {
    Parser.filter.parse(".key = 1").get.value shouldBe setPath(Vector(selectKey("key")), constNumber(1))
  }

  "parse path modifiers" in {
    Parser.filter.parse(".key |= . + 1").get.value shouldBe modifyPath(Vector(selectKey("key")), add(id, constNumber(1)))
  }

  "parse called filters" in {
    Parser.callFilter.parse("test").get.value shouldBe call("test", Vector.empty)
    Parser.callFilter.parse("test(.)").get.value shouldBe call("test", Vector(id))
    Parser.callFilter.parse("test(.;.)").get.value shouldBe call("test", Vector(id, id))
  }

  "parse piped filters" in {
    Parser.filter.parse(".key | .dang").get.value shouldBe
      (getPathS(selectKey("key")) | selectKey("dang"))
    Parser.filter.parse(".key | .dang | .hey").get.value shouldBe
      (getPathS(selectKey("key")) | getPathS(selectKey("dang")) | selectKey("hey"))
    Parser.filter.parse("(.key) | (.dang)").get.value shouldBe
      (getPathS(selectKey("key")) | selectKey("dang"))
    Parser.filter.parse("(.key) | (dang)").get.value shouldBe
      (getPathS(selectKey("key")) | call("dang"))
  }

  "parse ensequenced filters" in {
    Parser.filter.parse(".key, .dang").get.value shouldBe
      ensequence(selectKey("key"), selectKey("dang"))
  }

  "parse enlisted filters" in {
    Parser.enlistedFilter.parse("[.key, .dang]").get.value shouldBe
      enlist(ensequence(selectKey("key"), selectKey("dang")))
  }

  "parse definitions" in {
    Parser.definition.parse("def id: .;").get.value shouldBe
      Definition("id", Vector.empty, id)
    Parser.definition.parse("def id(f): .;").get.value shouldBe
      Definition("id", Vector("f"), id)
    Parser.definition.parse("def id(f; s): .;").get.value shouldBe
      Definition("id", Vector("f", "s"), id)
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
    "string key" in (Parser.enjectPair.parse("hello: id").get.value shouldBe Left("hello") -> call("id"))
    "escaped string key" in (Parser.enjectPair.parse("\"hello\": id").get.value shouldBe Left("hello") -> call("id"))
    "filter key" in (Parser.enjectPair.parse("(hello): id").get.value shouldBe Right(call("hello")) -> call("id"))
    "sugar" in (Parser.enjectPair.parse("user").get.value shouldBe Left("user") -> getPathS(selectKey("user")))
  }

  "parse enjected filters" in {
    Parser.filter.parse("{ sugar, user: \"user\", title: .titles[] }").get.value shouldBe
      enject(
        Vector(
          Left("sugar") -> getPathS(selectKey("sugar")),
          Left("user") -> constString("user"),
          Left("title") -> getPath(Vector(selectKey("titles"), collectResults))
        )
      )
  }

  "parse full programs" - {
    "with just a body" in (Parser.program.parse("id").get.value shouldBe
      Program[FilterAST](Vector.empty[Definition[FilterAST]], call("id")))
    "with small definitions" in (Parser.program.parse("def id: .; id").get.value shouldBe
      Program[FilterAST](Vector(Definition[FilterAST]("id", Vector.empty, id)), call("id")))
    "with whitespace at the start" in (Parser.program.parse(" .").get.value shouldBe
      Program[FilterAST](Vector.empty[Definition[FilterAST]], id))
    "with whitespace at the end" in (Parser.program.parse(". ").get.value shouldBe
      Program[FilterAST](Vector.empty[Definition[FilterAST]], id))
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
    "in a filter call" in (Parser.filter.parse("f($hello)").get.value shouldBe call("f", Vector(deref("hello"))))
  }

  "as bindings" - {
    "plain binding" in (Parser.asBinding.parse("$d as . in .").get.value shouldBe asBinding("d", id, id))
    "in a filter" in (Parser.filter.parse(". | $d as . in .").get.value shouldBe compose(id, asBinding("d", id, id)))
  }

}
