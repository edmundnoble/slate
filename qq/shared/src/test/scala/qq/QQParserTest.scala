package qq

import fastparse.core.ParseError
import qq.Definition
import utest._

object QQParserTest extends utest.TestSuite {

  val tests = this {

    "parse plain dots" - {
      QQParser.dot.parse(".").get.value ==> (())
      intercept[ParseError](QQParser.dot.parse("").get)
      ()
    }

    "parse selections" - {
      QQParser.selectKey.parse("key").get.value ==> QQFilter.selectKey("key")
      QQParser.selectIndex.parse("1").get.value ==> QQFilter.selectIndex(1)
      QQParser.selectIndex.parse("-1").get.value ==> QQFilter.selectIndex(-1)
    }

    "parse dottable filters" - {
      QQParser.dottableSimpleFilter.parse("[\"fuckeries \"]").get.value ==> QQFilter.selectKey("fuckeries ")
    }

    "parse dotted filters" - {
      QQParser.dottedFilter.parse(".").get.value ==> QQFilter.id
      QQParser.dottedFilter.parse(".[]").get.value ==> QQFilter.compose(QQFilter.id, QQFilter.collectResults(QQFilter.id))
      QQParser.dottedFilter.parse(".key").get.value ==> QQFilter.compose(QQFilter.id, QQFilter.selectKey("key"))
      QQParser.dottedFilter.parse(".[1]").get.value ==> QQFilter.compose(QQFilter.id, QQFilter.selectIndex(1))
      QQParser.dottedFilter.parse(".[-1]").get.value ==> QQFilter.compose(QQFilter.id, QQFilter.selectIndex(-1))
      QQParser.dottedFilter.parse(".[1][]").get.value ==> QQFilter.compose(QQFilter.id, QQFilter.collectResults(QQFilter.selectIndex(1)))
      QQParser.dottedFilter.parse(".key[]").get.value ==> QQFilter.compose(QQFilter.id, QQFilter.collectResults(QQFilter.selectKey("key")))
      QQParser.dottedFilter.parse(".key.otherkey.1.[1][].[1:3].[\"this key\"]").get.value ==>
        QQFilter.compose(QQFilter.compose(QQFilter.compose(QQFilter.compose(QQFilter.compose(QQFilter.compose(
          QQFilter.id, QQFilter.selectKey("key")), QQFilter.selectKey("otherkey")), QQFilter.selectKey("1")), QQFilter.collectResults(QQFilter.selectIndex(1))), QQFilter.selectRange(1, 3)), QQFilter.selectKey("this key"))
    }

    "parse called filters" - {
      QQParser.callFilter.parse("test").get.value ==> QQFilter.call("test")
    }

    "parse piped filters" - {
      QQParser.pipedFilter.parse(".key | .dang").get.value ==>
        QQFilter.compose(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")), QQFilter.compose(QQFilter.id, QQFilter.selectKey("dang")))
      QQParser.pipedFilter.parse("(.key) | (.dang)").get.value ==>
        QQFilter.compose(QQFilter.ensequence(List(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")))),
          QQFilter.ensequence(List(QQFilter.compose(QQFilter.id, QQFilter.selectKey("dang")))))
      QQParser.pipedFilter.parse("(.key) | (dang)").get.value ==>
        QQFilter.compose(QQFilter.ensequence(List(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")))),
          QQFilter.ensequence(List(QQFilter.call("dang"))))
    }

    "parse ensequenced filters" - {
      QQParser.ensequencedFilters.parse(".key, .dang").get.value ==>
        QQFilter.ensequence(List(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")), QQFilter.compose(QQFilter.id, QQFilter.selectKey("dang"))))
    }

    "parse enlisted filters" - {
      QQParser.enlistedFilter.parse("[.key, .dang]").get.value ==>
        QQFilter.enlist(QQFilter.ensequence(List(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")), QQFilter.compose(QQFilter.id, QQFilter.selectKey("dang")))))
    }

    "parse definitions" - {
      QQParser.definition.parse("def id: .;").get.value ==> Definition("id", Nil, QQFilter.ensequence(List(QQFilter.id)))
    }

    "parse full programs" - {
      QQParser.program.parse("id").get.value ==>(Seq(), QQFilter.ensequence(List(QQFilter.call("id"))))
      QQParser.program.parse("def id: .; id").get.value ==>
        (Seq(Definition("id", Nil, QQFilter.ensequence(List(QQFilter.id)))), QQFilter.ensequence(List(QQFilter.call("id"))))
    }

  }
}