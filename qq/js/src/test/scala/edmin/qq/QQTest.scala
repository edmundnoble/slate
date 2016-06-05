package edmin.qq

import edmin.qq.Util._
import edmin.qq.jsc.QQJS
import monix.execution.Scheduler.Implicits.global
import utest._

import scala.scalajs.js

object QQTest extends utest.TestSuite {
  override val tests = TestSuite {
    "identity program" - {
      val dictionary: js.Dictionary[Int] = js.Dictionary("1" -> 2, "3" -> 4)
      QQJS.run(".", List(dictionary)).runFuture.map(_ ==> List(dictionary))
    }

    "key program" - {
      val dictionary: js.Dictionary[js.Any] = js.Dictionary("lol" -> js.Dictionary[js.Any]("thing" -> 2), "wat" -> "ha")
      QQJS.run(".lol", List(dictionary)).runFuture.map(_.map(_.asInstanceOf[js.Dictionary[js.Any]].toMap) ==> List(Map[String, js.Any]("thing" -> 2)))
    }

    "ensequenced program" - {
      val dictionary: js.Dictionary[js.Any] = js.Dictionary("lol" -> js.Dictionary[js.Any]("thing" -> 2), "wat" -> "ha")
      QQJS.run(".lol, .wat", List(dictionary)).runFuture.map {
        case (obj: js.Object) :: (str: js.Any) :: Nil =>
          val dict = obj.asInstanceOf[js.Dictionary[js.Any]]
          dict.toMap ==> Map[String, js.Any]("thing" -> 2)
          str ==> "ha"
          ()
        case _ => assert(false)
      }
    }

    "enlisted program" - {
      val dictionary: js.Dictionary[js.Any] = js.Dictionary("lol" -> js.Dictionary[js.Any]("thing" -> 2), "wat" -> "ha")
      QQJS.run("[.lol, .wat]", List(dictionary)).runFuture.map {
        case (arr: js.Array[js.Any@unchecked]) :: Nil =>
          arr(0).asInstanceOf[js.Dictionary[js.Any]].toMap ==> Map[String, js.Any]("thing" -> 2)
          arr(1) ==> "ha"
          ()
        case _ => assert(false)
      }
    }

  }
}
