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

  }
}

