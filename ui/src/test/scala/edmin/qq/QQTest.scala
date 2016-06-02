package edmin.qq

import utest._

import scala.scalajs.js
import Util._
import monix.execution.Scheduler.Implicits.global

object QQTest extends utest.TestSuite {
  override val tests = TestSuite {
    "identity program" - {
      val dictionary: js.Dictionary[Int] = js.Dictionary("1" -> 2, "3" -> 4)
      QQ.run(".", List(dictionary)).runFuture.map(_ ==> List(dictionary))
    }

    "key program" - {
      val dictionary: js.Dictionary[js.Any] = js.Dictionary("lol" -> js.Dictionary[js.Any]("thing" -> 2), "wat" -> "ha")
      QQ.run(".lol", List(dictionary)).runFuture.map(_.map(_.asInstanceOf[js.Dictionary[js.Any]].toMap) ==> List(Map[String, js.Any]("thing" -> 2)))
    }

    "ensequenced program" - {
      val dictionary: js.Dictionary[js.Any] = js.Dictionary("lol" -> js.Dictionary[js.Any]("thing" -> 2), "wat" -> "ha")
      QQ.run(".lol, .wat", List(dictionary)).runFuture.map {
        case (dict: js.Dictionary[js.Any@unchecked]) :: (str: js.Any) :: Nil =>
        dict.toMap ==> Map[String, js.Any]("thing" -> 2)
        str ==> "ha"
        ()
      }
    }

    "enlisted program" - {
      val dictionary: js.Dictionary[js.Any] = js.Dictionary("lol" -> js.Dictionary[js.Any]("thing" -> 2), "wat" -> "ha")
      QQ.run("[.lol, .wat]", List(dictionary)).runFuture.map {
        case (arr: js.Array[js.Any@unchecked]) :: Nil =>
          arr(0).asInstanceOf[js.Dictionary[js.Any]].toMap ==> Map[String, js.Any]("thing" -> 2)
          arr(1) ==> "ha"
          ()
      }
    }
  }
}
