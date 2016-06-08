package qq

import QQAST._
import fastparse.core.ParseError
import utest._

object QQParserTest extends utest.TestSuite {

  val tests = this {

    "parse plain dots" - {
      QQParser.dot.parse(".").get.value ==> (())
      intercept[ParseError](QQParser.dot.parse("").get)
      ()
    }

    "parse selections" - {
      QQParser.selectKey.parse("key").get.value ==> SelectKey("key")
      QQParser.selectIndex.parse("1").get.value ==> SelectIndex(1)
      QQParser.selectIndex.parse("-1").get.value ==> SelectIndex(-1)
    }

    "parse dottable filters" - {
      QQParser.dottableSimpleFilter.parse("[\"fuckeries \"]").get.value ==> SelectKey("fuckeries ")
    }

    "parse dotted filters" - {
      QQParser.dottedFilter.parse(".").get.value ==> IdFilter
      QQParser.dottedFilter.parse(".[]").get.value ==> ComposeFilters(IdFilter, CollectResults(IdFilter))
      QQParser.dottedFilter.parse(".key").get.value ==> ComposeFilters(IdFilter, SelectKey("key"))
      QQParser.dottedFilter.parse(".[1]").get.value ==> ComposeFilters(IdFilter, SelectIndex(1))
      QQParser.dottedFilter.parse(".[-1]").get.value ==> ComposeFilters(IdFilter, SelectIndex(-1))
      QQParser.dottedFilter.parse(".[1][]").get.value ==> ComposeFilters(IdFilter, CollectResults(SelectIndex(1)))
      QQParser.dottedFilter.parse(".key[]").get.value ==> ComposeFilters(IdFilter, CollectResults(SelectKey("key")))
      QQParser.dottedFilter.parse(".key.otherkey.1.[1][].[1:3].[\"this key\"]").get.value ==>
        ComposeFilters(ComposeFilters(ComposeFilters(ComposeFilters(ComposeFilters(ComposeFilters(
          IdFilter, SelectKey("key")), SelectKey("otherkey")), SelectKey("1")), CollectResults(SelectIndex(1))), SelectRange(1, 3)), SelectKey("this key"))
    }

    "parse called filters" - {
      QQParser.callFilter.parse("test").get.value ==> CallFilter("test")
    }

    "parse piped filters" - {
      QQParser.pipedFilter.parse(".key | .dang").get.value ==>
        ComposeFilters(ComposeFilters(IdFilter, SelectKey("key")), ComposeFilters(IdFilter, SelectKey("dang")))
      QQParser.pipedFilter.parse("(.key) | (.dang)").get.value ==>
        ComposeFilters(EnsequenceFilters(List(ComposeFilters(IdFilter, SelectKey("key")))),
          EnsequenceFilters(List(ComposeFilters(IdFilter, SelectKey("dang")))))
      QQParser.pipedFilter.parse("(.key) | (dang)").get.value ==>
        ComposeFilters(EnsequenceFilters(List(ComposeFilters(IdFilter, SelectKey("key")))),
          EnsequenceFilters(List(CallFilter("dang"))))
    }

    "parse ensequenced filters" - {
      QQParser.ensequencedFilters.parse(".key, .dang").get.value ==>
        EnsequenceFilters(List(ComposeFilters(IdFilter, SelectKey("key")), ComposeFilters(IdFilter, SelectKey("dang"))))
    }

    "parse enlisted filters" - {
      QQParser.enlistedFilter.parse("[.key, .dang]").get.value ==>
        EnlistFilter(EnsequenceFilters(List(ComposeFilters(IdFilter, SelectKey("key")), ComposeFilters(IdFilter, SelectKey("dang")))))
    }

    "parse definitions" - {
      QQParser.definition.parse("def id: .;").get.value ==> Definition("id", Nil, EnsequenceFilters(List(IdFilter)))
    }

    "parse full programs" - {
      QQParser.program.parse("id").get.value ==> (Seq(), EnsequenceFilters(List(CallFilter("id"))))
      QQParser.program.parse("def id: .; id").get.value ==>
        (Seq(Definition("id", Nil, EnsequenceFilters(List(IdFilter)))), EnsequenceFilters(List(CallFilter("id"))))
    }

  }
}
