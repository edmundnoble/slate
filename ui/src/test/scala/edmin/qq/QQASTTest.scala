package edmin.qq

import utest._

import scala.scalajs.js

object QQASTTest extends utest.TestSuite {

  import monix.execution.Scheduler.Implicits.global

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
        _ <- compile(SelectKey("present"))(dict).runAsyncGetFirst map (_.get ==> 1)
        _ <- compile(SelectKey("absent"))(dict).runAsyncGetFirst map (_.get ==> null)
      } yield ()
    }

    "select index" - {
      val arr = js.Array(1, 2)
      def checkIndex(index: Int, result: Any) =
        compile(SelectIndex(index))(arr).runAsyncGetFirst map (_.get ==> result)
      for {
        _ <- checkIndex(-3, null)
        _ <- checkIndex(-2, 1)
        _ <- checkIndex(-1, 2)
        _ <- checkIndex(0, 1)
        _ <- checkIndex(1, 2)
        _ <- checkIndex(2, null)
      } yield ()
    }

    "select range" - {
      val arr = js.Array(1, 2, 3, 4)
      def checkRange(start: Int, end: Int, result: Array[Int]) =
        compile(SelectRange(start, end))(arr).runAsyncGetFirst map (_.get.asInstanceOf[js.Array[Int]].toArray ==> result)
      for {
        _ <- checkRange(0, 0, Array())
        _ <- checkRange(0, 1, Array(1))
        _ <- checkRange(0, 2, Array(1, 2))
        _ <- checkRange(0, 3, Array(1, 2, 3))
        _ <- checkRange(0, 4, Array(1, 2, 3, 4))
        _ <- checkRange(0, 5, Array(1, 2, 3, 4))
        _ <- checkRange(1, 5, Array(2, 3, 4))
      } yield ()
    }

    "collect results" - {
      def check(input: js.Any, output: Any) =
        compile(CollectResults(IdFilter))(input).foldLeftF[List[js.Any]](Nil)((x, y) => y :: x).runAsyncGetFirst map (_.get.reverse ==> output)

      for {
        _ <- check(js.Array(1, 2, 3, 4), List(1, 2, 3, 4))
        _ <- check(js.Dictionary("a" -> 1, "b" -> "c"), List(1, "c"))
      } yield ()
    }

  }
}
