package edmin

import edmin.QQ.QQAST._
import edmin.QQ._
import fastparse.core.ParseError
import fastparse.core.Parsed.Failure
import utest._

object QQTest extends utest.TestSuite {
  val tests = this {

    "parse plain dots" - {
      QQParser.dot.parse(".").get.value ==> (())
      intercept[ParseError](QQParser.dot.parse("").get)
      ()
    }

    "optimize simple compositions" - {
      QQAST.optimize(ComposeFilters(IdFilter, SelectKey("key"))) ==> SelectKey("key")
      QQAST.optimize(ComposeFilters(IdFilter, ComposeFilters(SelectKey("key"), IdFilter))) ==> SelectKey("key")
    }

    "parse selections" - {
      QQParser.selectKey.parse("key").get.value ==> SelectKey("key")
      QQParser.selectIndex.parse("1").get.value ==> SelectIndex(1)
    }

    "parse dottable filters" - {
      QQParser.dottableSimpleFilter.parse("[\"fuckeries \"]").get.value ==> SelectKey("fuckeries ")
    }

    "parse dotted filters" - {
      QQParser.dottedFilter.parse(".").get.value ==> IdFilter
      QQParser.dottedFilter.parse(".key").get.value ==> ComposeFilters(IdFilter, SelectKey("key"))
      QQParser.dottedFilter.parse(".[1]").get.value ==> ComposeFilters(IdFilter, SelectIndex(1))
      QQParser.dottedFilter.parse(".[1][]").get.value ==> ComposeFilters(IdFilter, CollectResults(SelectIndex(1)))
      QQParser.dottedFilter.parse(".key[]").get.value ==> ComposeFilters(IdFilter, CollectResults(SelectKey("key")))
      QQParser.dottedFilter.parse(".key.otherkey.1.[1][].[1:3].[\"this key\"]").get.value ==>
        ComposeFilters(ComposeFilters(ComposeFilters(ComposeFilters(ComposeFilters(ComposeFilters(
          IdFilter, SelectKey("key")), SelectKey("otherkey")), SelectKey("1")), CollectResults(SelectIndex(1))), SelectRange(1, 3)), SelectKey("this key"))
    }

    "parse piped filters" - {
      QQParser.pipedFilter.parse(".key | .dang").get.value ==>
        ComposeFilters(ComposeFilters(IdFilter, SelectKey("key")), ComposeFilters(IdFilter, SelectKey("dang")))
      QQParser.pipedFilter.parse("(.key) | (.dang)").get.value ==>
        ComposeFilters(EnsequenceFilters(ComposeFilters(IdFilter,SelectKey("key"))),
          EnsequenceFilters(ComposeFilters(IdFilter,SelectKey("dang"))))
    }

    "parse ensequenced filters" - {
      QQParser.ensequencedFilters.parse(".key, .dang").get.value ==>
        EnsequenceFilters(ComposeFilters(IdFilter, SelectKey("key")), ComposeFilters(IdFilter, SelectKey("dang")))
      QQParser.ensequencedFilters.parse(".key, .dang | ").get.value ==>
        EnsequenceFilters(ComposeFilters(IdFilter, SelectKey("key")), ComposeFilters(IdFilter, SelectKey("dang")))
    }

    "parse enlisted filters" - {
      QQParser.enlistedFilter.parse("[.key, .dang]").get.value ==>
        EnlistFilter(EnsequenceFilters(ComposeFilters(IdFilter, SelectKey("key")), ComposeFilters(IdFilter, SelectKey("dang"))))
    }

  }
}

