package edmin.qq

import QQAST._
import fastparse.core.ParseError
import utest._

import scala.scalajs.js
import scala.scalajs.js.Dictionary
import Util._
import monix.eval.Task
import monix.reactive.Observable
import monix.execution.Scheduler.Implicits.global

object QQTest extends utest.TestSuite {
  override val tests = TestSuite {
    "identity program" - {
      val dictionary: Dictionary[Int] = js.Dictionary("1" -> 2, "3" -> 4)
      QQ.run(".", List(dictionary)).runFuture.map(_ ==> List(dictionary))
    }
  }
}
