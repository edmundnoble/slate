package qq

import fastparse.core.ParseError
import qq.Definition
import utest._

import scalaz.{-\/, \/-}
import scalaz.syntax.std.list._
import shapeless._
import shapeless.ops.sized._
import shapeless.syntax.sized._

object QQParserTest extends utest.TestSuite with QQAsserts {

  val tests = this {
    val emptySized: List[String] = Nil

    "parse plain dots" - {
      QQParser.dot.parse(".").get
    }

    "parse selections" - {
      "select key" - {
        QQParser.selectKey.parse("key").get.value ===> QQFilter.selectKey("key")
      }
      "select index" - {
        QQParser.selectIndex.parse("1").get.value ===> QQFilter.selectIndex(1)
        QQParser.selectIndex.parse("-1").get.value ===> QQFilter.selectIndex(-1)
      }
    }

    "parse dottable filters" - {
      QQParser.dottableSimpleFilter.parse("[\"fuckeries \"]").get.value ===> QQFilter.selectKey("fuckeries ")
    }

    "parse dotted filters" - {
      QQParser.dottedFilter.parse(".").get.value ===> QQFilter.id
      QQParser.dottedFilter.parse(".[]").get.value ===> QQFilter.compose(QQFilter.id, QQFilter.collectResults(QQFilter.id))
      QQParser.dottedFilter.parse(".key").get.value ===> QQFilter.compose(QQFilter.id, QQFilter.selectKey("key"))
      QQParser.dottedFilter.parse(".[1]").get.value ===> QQFilter.compose(QQFilter.id, QQFilter.selectIndex(1))
      QQParser.dottedFilter.parse(".[-1]").get.value ===> QQFilter.compose(QQFilter.id, QQFilter.selectIndex(-1))
      QQParser.dottedFilter.parse(".[1][]").get.value ===> QQFilter.compose(QQFilter.id, QQFilter.collectResults(QQFilter.selectIndex(1)))
      QQParser.dottedFilter.parse(".key[]").get.value ===> QQFilter.compose(QQFilter.id, QQFilter.collectResults(QQFilter.selectKey("key")))
      QQParser.dottedFilter.parse(".key.otherkey.1.[1][].[1:3].[\"this key\"]").get.value ===>
        QQFilter.compose(QQFilter.compose(QQFilter.compose(QQFilter.compose(QQFilter.compose(QQFilter.compose(
          QQFilter.id, QQFilter.selectKey("key")), QQFilter.selectKey("otherkey")), QQFilter.selectKey("1")), QQFilter.collectResults(QQFilter.selectIndex(1))), QQFilter.selectRange(1, 3)), QQFilter.selectKey("this key"))
    }

    "parse called filters" - {
      QQParser.callFilter.parse("test").get.value ===> QQFilter.call("test", Nil)
      QQParser.callFilter.parse("test(.)").get.value ===> QQFilter.call("test", List(QQFilter.id))
      QQParser.callFilter.parse("test(.;.)").get.value ===> QQFilter.call("test", List(QQFilter.id, QQFilter.id))
    }

    "parse piped filters" - {
      QQParser.pipedFilter.parse(".key | .dang").get.value ===>
        QQFilter.compose(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")), QQFilter.compose(QQFilter.id, QQFilter.selectKey("dang")))
      QQParser.pipedFilter.parse("(.key) | (.dang)").get.value ===>
        QQFilter.compose(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")),
          QQFilter.compose(QQFilter.id, QQFilter.selectKey("dang")))
      QQParser.pipedFilter.parse("(.key) | (dang)").get.value ===>
        QQFilter.compose(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")),
          QQFilter.call("dang"))
    }

    "parse ensequenced filters" - {
      QQParser.ensequencedFilters.parse(".key, .dang").get.value ===>
        QQFilter.ensequence(List(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")), QQFilter.compose(QQFilter.id, QQFilter.selectKey("dang"))))
    }

    "parse enlisted filters" - {
      QQParser.enlistedFilter.parse("[.key, .dang]").get.value ===>
        QQFilter.enlist(QQFilter.ensequence(List(QQFilter.compose(QQFilter.id, QQFilter.selectKey("key")), QQFilter.compose(QQFilter.id, QQFilter.selectKey("dang")))))
    }

    "parse definitions" - {
      QQParser.definition.parse("def id: .;").get.value.asInstanceOf[Definition] ===> Definition("id", emptySized, QQFilter.id)
    }

    "parse constants" - {
      "integers" - {
        QQParser.filter.parse("1").get.value ===> QQFilter.constNumber(1)
        QQParser.filter.parse("4").get.value ===> QQFilter.constNumber(4)
      }
      "strings" - {
        QQParser.filter.parse(""""hello"""").get.value ===> QQFilter.constString("hello")
      }
    }

    "parse math operators" - {
      QQParser.filter.parse(". + .").get.value ===> QQFilter.addFilters(QQFilter.id, QQFilter.id)
      QQParser.filter.parse(". - .").get.value ===> QQFilter.subtractFilters(QQFilter.id, QQFilter.id)
      QQParser.filter.parse(". * .").get.value ===> QQFilter.multiplyFilters(QQFilter.id, QQFilter.id)
      QQParser.filter.parse(". / .").get.value ===> QQFilter.divideFilters(QQFilter.id, QQFilter.id)
      QQParser.filter.parse(". % .").get.value ===> QQFilter.moduloFilters(QQFilter.id, QQFilter.id)
    }

    "parse enjected pairs" - {
      "string key" - (QQParser.enjectPair.parse("hello: id").get.value ===> (-\/("hello") -> QQFilter.call("id")))
      "filter key" - (QQParser.enjectPair.parse("(hello): id").get.value ===> (\/-(QQFilter.call("hello")) -> QQFilter.call("id")))
      "sugar" - (QQParser.enjectPair.parse("user").get.value ===> (-\/("user") -> QQFilter.selectKey("user")))
    }

    "parse enjected filters" - {
      QQParser.enjectedFilter.parse("{ user, title: .titles[] }").get.value ===> QQFilter.enject(List(
        -\/("user") -> QQFilter.selectKey("user"),
        -\/("title") -> QQFilter.compose(QQFilter.id, QQFilter.collectResults(QQFilter.selectKey("titles")))
      ))
    }

    "parse full programs" - {
      QQParser.program.parse("id").get.value ===> ((Nil, QQFilter.call("id")))
      QQParser.program.parse("def id: .; id").get.value ===>
        ((List(Definition("id", emptySized, QQFilter.id)), QQFilter.call("id")))
      QQParser.program.parse(".titles[]").get.value ===> ((Nil, QQFilter.compose(QQFilter.id, QQFilter.collectResults(QQFilter.selectKey("titles")))))
    }

  }
}
