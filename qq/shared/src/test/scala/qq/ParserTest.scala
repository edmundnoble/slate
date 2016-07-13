package qq

import fastparse.core.ParseError

import scalaz.{-\/, \/-}
import scalaz.syntax.std.list._
import shapeless._
import shapeless.ops.sized._
import shapeless.syntax.sized._

class ParserTest extends QQTestSuite {

  import FilterDSL._

  val emptySized: List[String] = Nil

  "parse plain dots" in {
    Parser.dot.parse(".").get
  }

  "parse selections" - {
    "select key" in {
      Parser.selectKey.parse("key").get.value should equal(selectKey("key"))
    }
    "select index" in {
      Parser.selectIndex.parse("1").get.value should equal(selectIndex(1))
      Parser.selectIndex.parse("-1").get.value should equal(selectIndex(-1))
    }
  }

  "parse dottable filters" in {
    Parser.dottableSimpleFilter.parse("[\"filter \"]").get.value should equal(selectKey("filter "))
  }

  "parse dotted filters" in {
    Parser.dottedFilter.parse(".").get.value should equal(id)
    Parser.dottedFilter.parse(".[]").get.value should equal(compose(id, collectResults(id)))
    Parser.dottedFilter.parse(".key").get.value should equal(compose(id, selectKey("key")))
    Parser.dottedFilter.parse(".[1]").get.value should equal(compose(id, selectIndex(1)))
    Parser.dottedFilter.parse(".[-1]").get.value should equal(compose(id, selectIndex(-1)))
    Parser.dottedFilter.parse(".[1][]").get.value should equal(compose(id, collectResults(selectIndex(1))))
    Parser.dottedFilter.parse(".key[]").get.value should equal(compose(id, collectResults(selectKey("key"))))
    Parser.dottedFilter.parse(".key.otherkey.1.[1][].[1:3].[\"this key\"]").get.value should equal(
      compose(compose(compose(compose(compose(compose(
        id, selectKey("key")), selectKey("otherkey")), selectKey("1")), collectResults(selectIndex(1))), selectRange(1, 3)), selectKey("this key"))
    )
  }

  "parse called filters" in {
    Parser.callFilter.parse("test").get.value should equal(call("test", Nil))
    Parser.callFilter.parse("test(.)").get.value should equal(call("test", List(id)))
    Parser.callFilter.parse("test(.;.)").get.value should equal(call("test", List(id, id)))
  }

  "parse piped filters" in {
    Parser.filter.parse(".key | .dang").get.value should equal(
      compose(compose(id, selectKey("key")), compose(id, selectKey("dang"))))
    Parser.filter.parse("(.key) | (.dang)").get.value should equal(
      compose(compose(id, selectKey("key")),
        compose(id, selectKey("dang"))))
    Parser.filter.parse("(.key) | (dang)").get.value should equal(
      compose(compose(id, selectKey("key")),
        call("dang")))
  }

  "parse ensequenced filters" in {
    Parser.filter.parse(".key, .dang").get.value should equal(
      ensequence(compose(id, selectKey("key")), compose(id, selectKey("dang")))
    )
  }

  "parse enlisted filters" in {
    Parser.enlistedFilter.parse("[.key, .dang]").get.value should equal(
      enlist(ensequence(compose(id, selectKey("key")), compose(id, selectKey("dang"))))
    )
  }

  "parse definitions" in {
    Parser.definition.parse("def id: .;").get.value.asInstanceOf[Definition] should equal(
      Definition("id", emptySized, id)
    )
  }

  "parse constants" - {
    "integers" in {
      Parser.filter.parse("1").get.value should equal(constNumber(1))
      Parser.filter.parse("4").get.value should equal(constNumber(4))
    }
    "strings" in {
      Parser.filter.parse(""""hello"""").get.value should equal(constString("hello"))
    }
  }

  "parse math operators" in {
    Parser.filter.parse(". + .").get.value should equal(add(id, id))
    Parser.filter.parse(". - .").get.value should equal(subtract(id, id))
    Parser.filter.parse(". * .").get.value should equal(multiply(id, id))
    Parser.filter.parse(". / .").get.value should equal(divide(id, id))
    Parser.filter.parse(". % .").get.value should equal(modulo(id, id))
  }

  "parse enjected pairs" - {
    "string key" in (Parser.enjectPair.parse("hello: id").get.value should equal(-\/("hello") -> call("id")))
    "filter key" in (Parser.enjectPair.parse("(hello): id").get.value should equal(\/-(call("hello")) -> call("id")))
    "sugar" in (Parser.enjectPair.parse("user").get.value should equal(-\/("user") -> selectKey("user")))
  }

  "parse enjected filters" in {
    Parser.enjectedFilter.parse("{ sugar, user: \"user\", title: .titles[] }").get.value should equal(enject(List(
      -\/("sugar") -> selectKey("sugar"),
      -\/("user") -> constString("user"),
      -\/("title") -> compose(id, collectResults(selectKey("titles")))
    )))
  }

  "parse full programs" - {
    "with just a body" in (Parser.program.parse("id").get.value should equal((Nil, call("id"))))
    "with small definitions" in (Parser.program.parse("def id: .; id").get.value should equal(
      (List(Definition("id", emptySized, id)), call("id"))))
  }

  "parse strings" in {
    Parser.filter.parse(""""hello"""").get.value should equal(constString("hello"))
    Parser.filter.parse(""""\\"""").get.value should equal(constString("\\"))
  }

  "precedence" in {
    Parser.filter.parse("""., . | . + . * .""").get.value should equal(ensequence(id, compose(id, add(id, multiply(id, id)))))
    Parser.filter.parse(""". * . + . | ., .""").get.value should equal(ensequence(compose(add(multiply(id, id), id), id), id))
  }

}
