package qq

import fastparse.core.ParseError

import scalaz.{-\/, \/-}
import scalaz.syntax.std.list._
import shapeless._
import shapeless.ops.sized._
import shapeless.syntax.sized._

class ParserTest extends QQTestSuite {

  val emptySized: List[String] = Nil

  "parse plain dots" in {
    Parser.dot.parse(".").get
  }

  "parse selections" - {
    "select key" in {
      Parser.selectKey.parse("key").get.value should equal(Filter.selectKey("key"))
    }
    "select index" in {
      Parser.selectIndex.parse("1").get.value should equal(Filter.selectIndex(1))
      Parser.selectIndex.parse("-1").get.value should equal(Filter.selectIndex(-1))
    }
  }

  "parse dottable filters" in {
    Parser.dottableSimpleFilter.parse("[\"filter \"]").get.value should equal(Filter.selectKey("filter "))
  }

  "parse dotted filters" in {
    Parser.dottedFilter.parse(".").get.value should equal(Filter.id)
    Parser.dottedFilter.parse(".[]").get.value should equal(Filter.compose(Filter.id, Filter.collectResults(Filter.id)))
    Parser.dottedFilter.parse(".key").get.value should equal(Filter.compose(Filter.id, Filter.selectKey("key")))
    Parser.dottedFilter.parse(".[1]").get.value should equal(Filter.compose(Filter.id, Filter.selectIndex(1)))
    Parser.dottedFilter.parse(".[-1]").get.value should equal(Filter.compose(Filter.id, Filter.selectIndex(-1)))
    Parser.dottedFilter.parse(".[1][]").get.value should equal(Filter.compose(Filter.id, Filter.collectResults(Filter.selectIndex(1))))
    Parser.dottedFilter.parse(".key[]").get.value should equal(Filter.compose(Filter.id, Filter.collectResults(Filter.selectKey("key"))))
    Parser.dottedFilter.parse(".key.otherkey.1.[1][].[1:3].[\"this key\"]").get.value should equal(
      Filter.compose(Filter.compose(Filter.compose(Filter.compose(Filter.compose(Filter.compose(
        Filter.id, Filter.selectKey("key")), Filter.selectKey("otherkey")), Filter.selectKey("1")), Filter.collectResults(Filter.selectIndex(1))), Filter.selectRange(1, 3)), Filter.selectKey("this key"))
    )
  }

  "parse called filters" in {
    Parser.callFilter.parse("test").get.value should equal(Filter.call("test", Nil))
    Parser.callFilter.parse("test(.)").get.value should equal(Filter.call("test", List(Filter.id)))
    Parser.callFilter.parse("test(.;.)").get.value should equal(Filter.call("test", List(Filter.id, Filter.id)))
  }

  "parse piped filters" in {
    Parser.filter.parse(".key | .dang").get.value should equal(
      Filter.compose(Filter.compose(Filter.id, Filter.selectKey("key")), Filter.compose(Filter.id, Filter.selectKey("dang"))))
    Parser.filter.parse("(.key) | (.dang)").get.value should equal(
      Filter.compose(Filter.compose(Filter.id, Filter.selectKey("key")),
        Filter.compose(Filter.id, Filter.selectKey("dang"))))
    Parser.filter.parse("(.key) | (dang)").get.value should equal(
      Filter.compose(Filter.compose(Filter.id, Filter.selectKey("key")),
        Filter.call("dang")))
  }

  "parse ensequenced filters" in {
    Parser.filter.parse(".key, .dang").get.value should equal(
      Filter.ensequence(Filter.compose(Filter.id, Filter.selectKey("key")), Filter.compose(Filter.id, Filter.selectKey("dang")))
    )
  }

  "parse enlisted filters" in {
    Parser.enlistedFilter.parse("[.key, .dang]").get.value should equal(
      Filter.enlist(Filter.ensequence(Filter.compose(Filter.id, Filter.selectKey("key")), Filter.compose(Filter.id, Filter.selectKey("dang"))))
    )
  }

  "parse definitions" in {
    Parser.definition.parse("def id: .;").get.value.asInstanceOf[Definition] should equal(
      Definition("id", emptySized, Filter.id)
    )
  }

  "parse constants" - {
    "integers" in {
      Parser.filter.parse("1").get.value should equal(Filter.constNumber(1))
      Parser.filter.parse("4").get.value should equal(Filter.constNumber(4))
    }
    "strings" in {
      Parser.filter.parse(""""hello"""").get.value should equal(Filter.constString("hello"))
    }
  }

  "parse math operators" in {
    Parser.filter.parse(". + .").get.value should equal(Filter.add(Filter.id, Filter.id))
    Parser.filter.parse(". - .").get.value should equal(Filter.subtract(Filter.id, Filter.id))
    Parser.filter.parse(". * .").get.value should equal(Filter.multiply(Filter.id, Filter.id))
    Parser.filter.parse(". / .").get.value should equal(Filter.divide(Filter.id, Filter.id))
    Parser.filter.parse(". % .").get.value should equal(Filter.modulo(Filter.id, Filter.id))
  }

  "parse enjected pairs" - {
    "string key" in (Parser.enjectPair.parse("hello: id").get.value should equal(-\/("hello") -> Filter.call("id")))
    "filter key" in (Parser.enjectPair.parse("(hello): id").get.value should equal(\/-(Filter.call("hello")) -> Filter.call("id")))
    "sugar" in (Parser.enjectPair.parse("user").get.value should equal(-\/("user") -> Filter.selectKey("user")))
  }

  "parse enjected filters" in {
    Parser.enjectedFilter.parse("{ sugar, user: \"user\", title: .titles[] }").get.value should equal(Filter.enject(List(
      -\/("sugar") -> Filter.selectKey("sugar"),
      -\/("user") -> Filter.constString("user"),
      -\/("title") -> Filter.compose(Filter.id, Filter.collectResults(Filter.selectKey("titles")))
    )))
  }

  "parse full programs" - {
    "with just a body" in (Parser.program.parse("id").get.value should equal(((Nil, Filter.call("id")))))
    "with small definitions" in (Parser.program.parse("def id: .; id").get.value should equal(
      (List(Definition("id", emptySized, Filter.id)), Filter.call("id"))))
  }

  "parse strings" in {
    Parser.filter.parse(""""hello"""").get.value should equal(Filter.constString("hello"))
    Parser.filter.parse(""""\\"""").get.value should equal(Filter.constString("\\"))
  }

  "precedence" in {
    Parser.filter.parse("""., . | . + . * .""").get.value should equal(Filter.ensequence(Filter.id, Filter.compose(Filter.id, Filter.add(Filter.id, Filter.multiply(Filter.id, Filter.id)))))
    Parser.filter.parse(""". * . + . | ., .""").get.value should equal(Filter.ensequence(Filter.compose(Filter.add(Filter.multiply(Filter.id, Filter.id), Filter.id), Filter.id), Filter.id))
  }

}
