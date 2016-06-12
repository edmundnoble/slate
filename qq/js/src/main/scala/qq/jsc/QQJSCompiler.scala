package qq.jsc

import monix.eval.Task

import scala.scalajs.js
import scalaz.{-\/, NonEmptyList, \/, \/-}
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.std.list._
import scalaz.syntax.applicative._
import qq.QQCompiler
import qq.QQCompiler.QQRuntimeException
import qq.Util._

object QQJSCompiler extends QQCompiler {
  override type AnyTy = js.Any

  val taskOfListOfNull: Task[List[AnyTy]] = Task.now(List(null))
  val emptyArray: js.Array[js.Any] = new js.Array[js.Any](0)

  def enlistFilter(filter: CompiledFilter): CompiledFilter = { jsv: js.Any =>
    for {
      results <- filter(jsv)
    } yield js.Array(results: _*) :: Nil
  }

  def selectKey(key: String): CompiledFilter = {
    case f: js.Object =>
      f.asInstanceOf[js.Dictionary[js.Object]].get(key) match {
        case None => taskOfListOfNull
        case Some(v) => Task.now(v :: Nil)
      }
    case v =>
      Task.raiseError(new QQRuntimeException(s"Tried to select key $key in $v but it's not a dictionary"))
  }

  def selectIndex(index: Int): CompiledFilter = {
    case f: js.Array[js.Object@unchecked] =>
      if (index >= -f.length) {
        if (index >= 0 && index < f.length) {
          Task.now(f(index) :: Nil)
        } else if (index < 0) {
          Task.now(f(f.length + index) :: Nil)
        } else {
          taskOfListOfNull
        }
      } else {
        taskOfListOfNull
      }
    case v =>
      Task.raiseError(new QQRuntimeException(s"Tried to select index $index in $v but it's not an array"))
  }

  def selectRange(start: Int, end: Int): CompiledFilter = {
    case f: js.Array[js.Object@unchecked] =>
      if (start < end && start < f.length) {
        Task.now(f.jsSlice(start, end) :: Nil)
      } else {
        Task.now(emptyArray :: Nil)
      }
  }

  def collectResults(f: CompiledFilter): CompiledFilter = { jsv: js.Any =>
    f(jsv).flatMap {
      _.traverseM {
        case arr: js.Array[js.Any@unchecked] =>
          Task.now(arr.toList)
        case dict: js.Object =>
          Task.now(dict.asInstanceOf[js.Dictionary[js.Object]].map(_._2)(collection.breakOut))
        case v =>
          Task.raiseError(new QQRuntimeException(s"Tried to flatten $v but it's not an array"))
      }
    }
  }

  override def enjectFilter(obj: List[(\/[String, CompiledFilter], CompiledFilter)]): CompiledFilter = { jsv: AnyTy =>
    for {
      kvPairs <- obj.traverse {
        case (\/-(filterKey), filterValue) =>
          for {
            keyResults <- filterKey(jsv)
            valueResults <- filterValue(jsv)
            keyValuePairs <- keyResults.traverse {
              case keyString: js.Any if keyString.isInstanceOf[String] =>
                Task.now(valueResults.map(keyString.asInstanceOf[String] -> _))
              case k =>
                Task.raiseError(new QQRuntimeException(s"Tried to use $k as a key for an object but it's not a string"))
            }
          } yield keyValuePairs
        case (-\/(filterName), filterValue) =>
          for {
            valueResults <- filterValue(jsv)
          } yield valueResults.map(filterName -> _) :: Nil
      }
      kvPairsProducts = kvPairs.map(_.flatten) <^> { case NonEmptyList(h, l) => l.foldLeft(h :: Nil)(prod) }
    } yield kvPairsProducts.map(js.Dictionary[js.Any](_: _*))
  }

}

