package qq

import fastparse.core.ParseError
import qq.QQAST.{Definition, QQProgram}
import utest._

object QQParserTest extends utest.TestSuite {

  val tests = this {

    "parse plain dots" - {
      QQParser.dot.parse(".").get.value ==> (())
      intercept[ParseError](QQParser.dot.parse("").get)
      ()
    }

    "parse selections" - {
      QQParser.selectKey.parse("key").get.value ==> QQProgram.selectKey("key")
      QQParser.selectIndex.parse("1").get.value ==> QQProgram.selectIndex(1)
      QQParser.selectIndex.parse("-1").get.value ==> QQProgram.selectIndex(-1)
    }

    "parse dottable filters" - {
      QQParser.dottableSimpleFilter.parse("[\"fuckeries \"]").get.value ==> QQProgram.selectKey("fuckeries ")
    }

    "parse dotted filters" - {
      QQParser.dottedFilter.parse(".").get.value ==> QQProgram.id
      QQParser.dottedFilter.parse(".[]").get.value ==> QQProgram.compose(QQProgram.id, QQProgram.collectResults(QQProgram.id))
      QQParser.dottedFilter.parse(".key").get.value ==> QQProgram.compose(QQProgram.id, QQProgram.selectKey("key"))
      QQParser.dottedFilter.parse(".[1]").get.value ==> QQProgram.compose(QQProgram.id, QQProgram.selectIndex(1))
      QQParser.dottedFilter.parse(".[-1]").get.value ==> QQProgram.compose(QQProgram.id, QQProgram.selectIndex(-1))
      QQParser.dottedFilter.parse(".[1][]").get.value ==> QQProgram.compose(QQProgram.id, QQProgram.collectResults(QQProgram.selectIndex(1)))
      QQParser.dottedFilter.parse(".key[]").get.value ==> QQProgram.compose(QQProgram.id, QQProgram.collectResults(QQProgram.selectKey("key")))
      QQParser.dottedFilter.parse(".key.otherkey.1.[1][].[1:3].[\"this key\"]").get.value ==>
        QQProgram.compose(QQProgram.compose(QQProgram.compose(QQProgram.compose(QQProgram.compose(QQProgram.compose(
          QQProgram.id, QQProgram.selectKey("key")), QQProgram.selectKey("otherkey")), QQProgram.selectKey("1")), QQProgram.collectResults(QQProgram.selectIndex(1))), QQProgram.selectRange(1, 3)), QQProgram.selectKey("this key"))
    }

    "parse called filters" - {
      QQParser.callFilter.parse("test").get.value ==> QQProgram.call("test")
    }

    "parse piped filters" - {
      QQParser.pipedFilter.parse(".key | .dang").get.value ==>
        QQProgram.compose(QQProgram.compose(QQProgram.id, QQProgram.selectKey("key")), QQProgram.compose(QQProgram.id, QQProgram.selectKey("dang")))
      QQParser.pipedFilter.parse("(.key) | (.dang)").get.value ==>
        QQProgram.compose(QQProgram.ensequence(List(QQProgram.compose(QQProgram.id, QQProgram.selectKey("key")))),
          QQProgram.ensequence(List(QQProgram.compose(QQProgram.id, QQProgram.selectKey("dang")))))
      QQParser.pipedFilter.parse("(.key) | (dang)").get.value ==>
        QQProgram.compose(QQProgram.ensequence(List(QQProgram.compose(QQProgram.id, QQProgram.selectKey("key")))),
          QQProgram.ensequence(List(QQProgram.call("dang"))))
    }

    "parse ensequenced filters" - {
      QQParser.ensequencedFilters.parse(".key, .dang").get.value ==>
        QQProgram.ensequence(List(QQProgram.compose(QQProgram.id, QQProgram.selectKey("key")), QQProgram.compose(QQProgram.id, QQProgram.selectKey("dang"))))
    }

    "parse enlisted filters" - {
      QQParser.enlistedFilter.parse("[.key, .dang]").get.value ==>
        QQProgram.enlist(QQProgram.ensequence(List(QQProgram.compose(QQProgram.id, QQProgram.selectKey("key")), QQProgram.compose(QQProgram.id, QQProgram.selectKey("dang")))))
    }

    "parse definitions" - {
      QQParser.definition.parse("def id: .;").get.value ==> Definition("id", Nil, QQProgram.ensequence(List(QQProgram.id)))
    }

    "parse full programs" - {
      QQParser.program.parse("id").get.value ==>(Seq(), QQProgram.ensequence(List(QQProgram.call("id"))))
      QQParser.program.parse("def id: .; id").get.value ==>
        (Seq(Definition("id", Nil, QQProgram.ensequence(List(QQProgram.id)))), QQProgram.ensequence(List(QQProgram.call("id"))))
    }

  }
}
