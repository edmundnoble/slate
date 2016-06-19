package qq.jsc

import monix.eval.Task

import scala.scalajs.js
import scalaz.{-\/, Applicative, NonEmptyList, Traverse, \/, \/-}
import scalaz.std.list._
import scalaz.std.map._
import scalaz.syntax.traverse._
import scalaz.syntax.either._
import scalaz.syntax.std.list._
import scalaz.syntax.std.map._
import scalaz.syntax.applicative._
import qq.Compiler
import qq.Compiler.QQRuntimeException
import qq.Util._

import scala.scalajs.js.{Any, Dictionary}

object JSCompiler extends Compiler {
  override type AnyTy = js.Any

  val taskOfListOfNull: Task[List[AnyTy]] = Task.now(List(null))
  val emptyArray: js.Array[js.Any] = new js.Array[js.Any](0)

  override def constNumber(num: Double): CompiledFilter = {
    _ => Task.now(num :: Nil)
  }

  override def constString(str: String): CompiledFilter = {
    _ => Task.now(str :: Nil)
  }

  def addJsValues(first: js.Any, second: js.Any): Task[js.Any] = (first, second) match {
    case (f, s) if f.isInstanceOf[Double] && s.isInstanceOf[Double] =>
      Task.now(f.asInstanceOf[Double] + s.asInstanceOf[Double])
    case (f, s) if f.isInstanceOf[String] && s.isInstanceOf[String] =>
      Task.now(f.asInstanceOf[String] + s.asInstanceOf[String])
    case (f: js.Array[js.Any@unchecked], s: js.Array[js.Any@unchecked]) =>
      Task.now(f.concat(s))
    case (f: js.Object, s: js.Object) =>
      val firstMap = f.asInstanceOf[js.Dictionary[js.Any]].toMap
      val secondMap = s.asInstanceOf[Dictionary[Any]].toMap
      Task.now(js.Dictionary((firstMap ++ secondMap).toSeq: _*))
    case (f, s) =>
      Task.raiseError(new QQRuntimeException(s"can't add $f and $s"))
  }

  def subtractJsValues(first: js.Any, second: js.Any): Task[js.Any] = (first, second) match {
    case (f, s) if f.isInstanceOf[Double] && s.isInstanceOf[Double] =>
      Task.now(f.asInstanceOf[Double] - s.asInstanceOf[Double])
    case (f: js.Array[js.Any@unchecked], s: js.Array[js.Any@unchecked]) =>
      Task.now(f.filter(!s.contains(_)))
    case (f: js.Object, s: js.Object) =>
      val contents: Seq[(String, Any)] = (f.asInstanceOf[Dictionary[Any]].toMap -- s.asInstanceOf[Dictionary[Any]].toMap.keySet).toSeq
      Task.now(js.Dictionary(contents: _*))
    case (f, s) =>
      Task.raiseError(new QQRuntimeException(s"can't subtract $f and $s"))
  }

  def multiplyJsValues(first: js.Any, second: js.Any): Task[js.Any] = (first, second) match {
    case (f, s) if f.isInstanceOf[Double] && s.isInstanceOf[Double] => Task.now(f.asInstanceOf[Double] * s.asInstanceOf[Double])
    case (f, s) if f.isInstanceOf[String] && s.isInstanceOf[Int] =>
      Task.now(if (s.asInstanceOf[Int] == 0) null else f.asInstanceOf[String] * s.asInstanceOf[Int])
    case (f: js.Object, s: js.Object) =>
      val firstMap = f.asInstanceOf[js.Dictionary[js.Any]].toMap.mapValues(Task.now)
      val secondMap = s.asInstanceOf[Dictionary[Any]].toMap.mapValues(Task.now)
      firstMap.unionWith(secondMap) { (f, s) => (f |@| s)(addJsValues(_, _)).flatten[js.Any] }.sequence.map(o => js.Dictionary(o.toSeq: _*))
    case (f, s) =>
      Task.raiseError(new QQRuntimeException(s"can't multiply $f and $s"))
  }

  def divideJsValues(first: js.Any, second: js.Any): Task[js.Any] = (first, second) match {
    case (f, s) if f.isInstanceOf[Double] && s.isInstanceOf[Double] => Task.now(f.asInstanceOf[Double] / s.asInstanceOf[Double])
    case (f, s) =>
      Task.raiseError(new QQRuntimeException(s"can't divide $f by $s"))
  }

  def moduloJsValues(first: js.Any, second: js.Any): Task[js.Any] = (first, second) match {
    case (f, s) if f.isInstanceOf[Double] && s.isInstanceOf[Double] => Task.now(f.asInstanceOf[Double] % s.asInstanceOf[Double])
    case (f, s) =>
      Task.raiseError(new QQRuntimeException(s"can't modulo $f by $s"))
  }

  override def enlistFilter(filter: CompiledFilter): CompiledFilter = { jsv: js.Any =>
    for {
      results <- filter(jsv)
    } yield js.Array(results: _*) :: Nil
  }

  override def selectKey(key: String): CompiledFilter = {
    case f: js.Object =>
      f.asInstanceOf[js.Dictionary[js.Object]].get(key) match {
        case None => taskOfListOfNull
        case Some(v) => Task.now(v :: Nil)
      }
    case v =>
      Task.raiseError(new QQRuntimeException(s"Tried to select key $key in $v but it's not a dictionary"))
  }

  override def selectIndex(index: Int): CompiledFilter = {
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

  override def selectRange(start: Int, end: Int): CompiledFilter = {
    case f: js.Array[js.Object@unchecked] =>
      if (start < end && start < f.length) {
        Task.now(f.jsSlice(start, end) :: Nil)
      } else {
        Task.now(emptyArray :: Nil)
      }
  }

  override def collectResults(f: CompiledFilter): CompiledFilter = { jsv: js.Any =>
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
      kvPairsProducts = kvPairs.map(_.flatten) <^> { case NonEmptyList(h, l) => l.foldLeft(h :: Nil)(withPrefixes) }
    } yield kvPairsProducts.map(js.Dictionary[js.Any](_: _*))
  }

  override def platformPrelude = JSPrelude

}

