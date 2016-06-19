package qq

import fastparse.core.ParseError
import qq.Definition
import utest._

import scalaz.{-\/, \/-}
import scalaz.syntax.std.list._
import shapeless._
import shapeless.ops.sized._
import shapeless.syntax.sized._

object ParserTest extends utest.TestSuite with Asserts {

  val tests = this {
    val emptySized: List[String] = Nil

    "parse plain dots" - {
      Parser.dot.parse(".").get
    }

    "parse selections" - {
      "select key" - {
        Parser.selectKey.parse("key").get.value ===> Filter.selectKey("key")
      }
      "select index" - {
        Parser.selectIndex.parse("1").get.value ===> Filter.selectIndex(1)
        Parser.selectIndex.parse("-1").get.value ===> Filter.selectIndex(-1)
      }
    }

    "parse dottable filters" - {
      Parser.dottableSimpleFilter.parse("[\"filter \"]").get.value ===> Filter.selectKey("filter ")
    }

    "parse dotted filters" - {
      Parser.dottedFilter.parse(".").get.value ===> Filter.id
      Parser.dottedFilter.parse(".[]").get.value ===> Filter.compose(Filter.id, Filter.collectResults(Filter.id))
      Parser.dottedFilter.parse(".key").get.value ===> Filter.compose(Filter.id, Filter.selectKey("key"))
      Parser.dottedFilter.parse(".[1]").get.value ===> Filter.compose(Filter.id, Filter.selectIndex(1))
      Parser.dottedFilter.parse(".[-1]").get.value ===> Filter.compose(Filter.id, Filter.selectIndex(-1))
      Parser.dottedFilter.parse(".[1][]").get.value ===> Filter.compose(Filter.id, Filter.collectResults(Filter.selectIndex(1)))
      Parser.dottedFilter.parse(".key[]").get.value ===> Filter.compose(Filter.id, Filter.collectResults(Filter.selectKey("key")))
      Parser.dottedFilter.parse(".key.otherkey.1.[1][].[1:3].[\"this key\"]").get.value ===>
        Filter.compose(Filter.compose(Filter.compose(Filter.compose(Filter.compose(Filter.compose(
          Filter.id, Filter.selectKey("key")), Filter.selectKey("otherkey")), Filter.selectKey("1")), Filter.collectResults(Filter.selectIndex(1))), Filter.selectRange(1, 3)), Filter.selectKey("this key"))
    }

    "parse called filters" - {
      Parser.callFilter.parse("test").get.value ===> Filter.call("test", Nil)
      Parser.callFilter.parse("test(.)").get.value ===> Filter.call("test", List(Filter.id))
      Parser.callFilter.parse("test(.;.)").get.value ===> Filter.call("test", List(Filter.id, Filter.id))
    }

    "parse piped filters" - {
      Parser.pipedFilter.parse(".key | .dang").get.value ===>
        Filter.compose(Filter.compose(Filter.id, Filter.selectKey("key")), Filter.compose(Filter.id, Filter.selectKey("dang")))
      Parser.pipedFilter.parse("(.key) | (.dang)").get.value ===>
        Filter.compose(Filter.compose(Filter.id, Filter.selectKey("key")),
          Filter.compose(Filter.id, Filter.selectKey("dang")))
      Parser.pipedFilter.parse("(.key) | (dang)").get.value ===>
        Filter.compose(Filter.compose(Filter.id, Filter.selectKey("key")),
          Filter.call("dang"))
    }

    "parse ensequenced filters" - {
      Parser.ensequencedFilters.parse(".key, .dang").get.value ===>
        Filter.ensequence(List(Filter.compose(Filter.id, Filter.selectKey("key")), Filter.compose(Filter.id, Filter.selectKey("dang"))))
    }

    "parse enlisted filters" - {
      Parser.enlistedFilter.parse("[.key, .dang]").get.value ===>
        Filter.enlist(Filter.ensequence(List(Filter.compose(Filter.id, Filter.selectKey("key")), Filter.compose(Filter.id, Filter.selectKey("dang")))))
    }

    "parse definitions" - {
      Parser.definition.parse("def id: .;").get.value.asInstanceOf[Definition] ===> Definition("id", emptySized, Filter.id)
    }

    "parse constants" - {
      "integers" - {
        Parser.filter.parse("1").get.value ===> Filter.constNumber(1)
        Parser.filter.parse("4").get.value ===> Filter.constNumber(4)
      }
      "strings" - {
        Parser.filter.parse(""""hello"""").get.value ===> Filter.constString("hello")
      }
    }

    "parse math operators" - {
      Parser.filter.parse(". + .").get.value ===> Filter.add(Filter.id, Filter.id)
      Parser.filter.parse(". - .").get.value ===> Filter.subtract(Filter.id, Filter.id)
      Parser.filter.parse(". * .").get.value ===> Filter.multiply(Filter.id, Filter.id)
      Parser.filter.parse(". / .").get.value ===> Filter.divide(Filter.id, Filter.id)
      Parser.filter.parse(". % .").get.value ===> Filter.modulo(Filter.id, Filter.id)
    }

    "parse enjected pairs" - {
      "string key" - (Parser.enjectPair.parse("hello: id").get.value ===> (-\/("hello") -> Filter.call("id")))
      "filter key" - (Parser.enjectPair.parse("(hello): id").get.value ===> (\/-(Filter.call("hello")) -> Filter.call("id")))
      "sugar" - (Parser.enjectPair.parse("user").get.value ===> (-\/("user") -> Filter.selectKey("user")))
    }

    "parse enjected filters" - {
      Parser.enjectedFilter.parse("{ sugar, user: \"user\", title: .titles[] }").get.value ===> Filter.enject(List(
        -\/("sugar") -> Filter.selectKey("sugar"),
        -\/("user") -> Filter.constString("user"),
        -\/("title") -> Filter.compose(Filter.id, Filter.collectResults(Filter.selectKey("titles")))
      ))
    }

    "parse full programs" - {
      "with just a body" - (Parser.program.parse("id").get.value ===> ((Nil, Filter.call("id"))))
      "with small definitions" - (Parser.program.parse("def id: .; id").get.value ===>
        ((List(Definition("id", emptySized, Filter.id)), Filter.call("id"))))
    }

    "parse strings" - {
      Parser.basicFilter.parse(""""hello"""").get.value ===> Filter.constString("hello")
    }

  }
}
