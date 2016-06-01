package edmin.qq

import utest._

import scala.concurrent.Future
import scala.scalajs.js
import Util._

import monix.execution.Scheduler.Implicits.global

object QQASTTest extends utest.TestSuite {

  val tests = this {

    import QQAST._

    "optimize simple compositions" - {
      optimize(ComposeFilters(IdFilter, SelectKey("key"))) ==> SelectKey("key")
      optimize(ComposeFilters(IdFilter, ComposeFilters(SelectKey("key"), IdFilter))) ==> SelectKey("key")
    }

    "optimize pipes and dots to the same thing" - {
      optimize(QQParser.ensequencedFilters.parse(".key | .dang").get.value) ==> ComposeFilters(SelectKey("key"), SelectKey("dang"))
      optimize(QQParser.ensequencedFilters.parse(".key.dang").get.value) ==> ComposeFilters(SelectKey("key"), SelectKey("dang"))
    }

    "select keys" - {
      val dict = js.Dictionary("present" -> 1)
      for {
        _ <- compile(Nil, SelectKey("present")).getOrElse(???).runAttempt.value(dict).runFuture map (_ ==> List(1))
        _ <- compile(Nil, SelectKey("absent")).getOrElse(???).runAttempt.value(dict).runFuture map (_ ==> List(null))
      } yield ()
    }

    "select index" - {
      val arr = js.Array(1, 2)
      def checkIndex(index: Int, result: Any) =
        compile(Nil, SelectIndex(index)).getOrElse(???).runAttempt.value(arr).runFuture map (_ ==> List(result))
      for {
        _ <- checkIndex(-3, null)
        _ <- checkIndex(-2, 1)
        _ <- checkIndex(-1, 2)
        _ <- checkIndex(0, 1)
        _ <- checkIndex(1, 2)
        _ <- checkIndex(2, null)
      } yield ()
    }

    "id" - {
      val dict = js.Dictionary("present" -> 1)
      for {
        _ <- compile(Nil, IdFilter).getOrElse(???).runAttempt.value(dict).runFuture.map(_ ==> List(dict))
      } yield ()
    }

    "select range" - {
      val arr = js.Array(1, 2, 3, 4)
      def checkRange(start: Int, end: Int, result: List[Int]) = {
        compile(Nil, SelectRange(start, end)).getOrElse(???).runAttempt.value(arr).runFuture map (_.map(_.asInstanceOf[js.Array[Int]].toList) ==> List(result))
      }
      for {
        _ <- checkRange(0, 0, List())
        _ <- checkRange(0, 1, List(1))
        _ <- checkRange(0, 2, List(1, 2))
        _ <- checkRange(0, 3, List(1, 2, 3))
        _ <- checkRange(0, 4, List(1, 2, 3, 4))
        _ <- checkRange(0, 5, List(1, 2, 3, 4))
        _ <- checkRange(1, 5, List(2, 3, 4))
      } yield ()
    }

    "collect results" - {
      def check(input: js.Any, output: Any) =
        compile(Nil, CollectResults(IdFilter)).getOrElse(???).runAttempt.value(input).runFuture map (_ ==> output)

      for {
        _ <- check(js.Array(1, 2, 3, 4), List(1, 2, 3, 4))
        _ <- check(js.Dictionary("a" -> 1, "b" -> "c"), List(1, "c"))
      } yield ()
    }

  }
}
