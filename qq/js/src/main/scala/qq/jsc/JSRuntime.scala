package qq.jsc

import monix.eval.Task
import qq.{QQRuntime, QQRuntimeException}
import qq.QQCompiler.CompiledFilter
import qq.Util._

import scala.scalajs.js
import scala.scalajs.js.Dictionary
import scalaz.std.list._
import scalaz.std.map._
import scalaz.syntax.std.list._
import scalaz.syntax.std.map._
import scalaz.syntax.traverse._
import scalaz.{-\/, NonEmptyList, \/, \/-}

// This is a QQ runtime which executes all operations in native Javascript
object JSRuntime extends QQRuntime[AnyRef] {

  val taskOfListOfNull: Task[List[AnyRef]] = Task.now(List(null))
  val emptyArray: js.Array[AnyRef] = new js.Array[AnyRef](0)

  override def constNumber(num: Double): CompiledFilter[AnyRef] = _ => Task.now(Double.box(num) :: Nil)

  override def constString(str: String): CompiledFilter[AnyRef] = _ => Task.now(str :: Nil)

  def addJsValues(first: AnyRef, second: AnyRef): Task[AnyRef] = (first, second) match {
    case (f: java.lang.Double, s: java.lang.Double) =>
      Task.now(Double.box(f.doubleValue() + s.doubleValue()))
    case (f: String, s: String) =>
      Task.now(f + s)
    case (f: js.Array[AnyRef@unchecked], s: js.Array[AnyRef@unchecked]) =>
      Task.now(f.concat(s))
    case (f: js.Object, s: js.Object) =>
      val firstMap = f.asInstanceOf[js.Dictionary[AnyRef]].toMap
      val secondMap = s.asInstanceOf[js.Dictionary[AnyRef]].toMap
      Task.now(js.Dictionary((firstMap ++ secondMap).toSeq: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException(s"can't add $f and $s"))
  }

  def subtractJsValues(first: AnyRef, second: AnyRef): Task[AnyRef] = (first, second) match {
    case (f: java.lang.Double, s: java.lang.Double) =>
      Task.now(Double.box(f.doubleValue() - s.doubleValue()))
    case (f: js.Array[AnyRef@unchecked], s: js.Array[AnyRef@unchecked]) =>
      Task.now(f.filter(!s.contains(_)))
    case (f: js.Object, s: js.Object) =>
      val contents: Seq[(String, AnyRef)] = (f.asInstanceOf[Dictionary[AnyRef]].toMap -- s.asInstanceOf[Dictionary[AnyRef]].toMap.keySet).toSeq
      Task.now(js.Dictionary(contents: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException(s"can't subtract $f and $s"))
  }

  def multiplyJsValues(first: AnyRef, second: AnyRef): Task[AnyRef] = (first, second) match {
    case (f: java.lang.Double, s: java.lang.Double) =>
      Task.now(Double.box(f.doubleValue() * s.doubleValue()))
    case (f: String, s: java.lang.Integer) =>
      Task.now(if (s.intValue() == 0) null else f * s.intValue())
    case (f: js.Object, s: js.Object) =>
      val firstMap = f.asInstanceOf[js.Dictionary[AnyRef]].toMap.mapValues(Task.now)
      val secondMap = s.asInstanceOf[Dictionary[AnyRef]].toMap.mapValues(Task.now)
      firstMap.unionWith(secondMap) { (f, s) => Task.mapBoth(f, s)(addJsValues).flatten }.sequence.map(o => js.Dictionary(o.toSeq: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException(s"can't multiply $f and $s"))
  }

  def divideJsValues(first: AnyRef, second: AnyRef): Task[AnyRef] = (first, second) match {
    case (f: java.lang.Double, s: java.lang.Double) =>
      Task.now(Double.box(f.doubleValue() / s.doubleValue()))
    case (f, s) =>
      Task.raiseError(QQRuntimeException(s"can't divide $f by $s"))
  }

  def moduloJsValues(first: AnyRef, second: AnyRef): Task[AnyRef] = (first, second) match {
    case (f: java.lang.Double, s: java.lang.Double) =>
      Task.now(Double.box(f.doubleValue() % s.doubleValue()))
    case (f, s) =>
      Task.raiseError(QQRuntimeException(s"can't modulo $f by $s"))
  }

  override def enlistFilter(filter: CompiledFilter[AnyRef]): CompiledFilter[AnyRef] = { jsv: AnyRef =>
    for {
      results <- filter(jsv)
    } yield js.Array(results: _*) :: Nil
  }

  override def selectKey(key: String): CompiledFilter[AnyRef] = {
    case f: js.Object =>
      f.asInstanceOf[js.Dictionary[AnyRef]].get(key) match {
        case None => taskOfListOfNull
        case Some(v) => Task.now(v :: Nil)
      }
    case v =>
      Task.raiseError(QQRuntimeException(s"Tried to select key $key in $v but it's not a dictionary"))
  }

  override def selectIndex(index: Int): CompiledFilter[AnyRef] = {
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
      Task.raiseError(QQRuntimeException(s"Tried to select index $index in $v but it's not an array"))
  }

  override def selectRange(start: Int, end: Int): CompiledFilter[AnyRef] = {
    case f: js.Array[js.Object@unchecked] =>
      if (start < end && start < f.length) {
        Task.now(f.jsSlice(start, end) :: Nil)
      } else {
        Task.now(emptyArray :: Nil)
      }
  }

  override def collectResults(f: CompiledFilter[AnyRef]): CompiledFilter[AnyRef] = { jsv: AnyRef =>
    f(jsv).flatMap {
      _.traverseM {
        case arr: js.Array[AnyRef@unchecked] =>
          Task.now(arr.toList)
        case dict: js.Object =>
          Task.now(dict.asInstanceOf[js.Dictionary[js.Object]].map(_._2)(collection.breakOut))
        case v =>
          Task.raiseError(QQRuntimeException(s"Tried to flatten $v but it's not an array"))
      }
    }
  }

  override def enjectFilter(obj: List[(\/[String, CompiledFilter[AnyRef]], CompiledFilter[AnyRef])]): CompiledFilter[AnyRef] = { jsv: AnyRef =>
    for {
      kvPairs <- obj.traverse[Task, List[List[(String, AnyRef)]]] {
        case (\/-(filterKey), filterValue) =>
          for {
            keyResults <- filterKey(jsv)
            valueResults <- filterValue(jsv)
            keyValuePairs <- keyResults.traverse[Task, List[(String, AnyRef)]] {
              case keyString: String =>
                Task.now(valueResults.map(keyString -> _))
              case k =>
                Task.raiseError(QQRuntimeException(s"Tried to use $k as a key for an object but it's not a string"))
            }
          } yield keyValuePairs
        case (-\/(filterName), filterValue) =>
          for {
            valueResults <- filterValue(jsv)
          } yield valueResults.map(filterName -> _) :: Nil
      }
      kvPairsProducts = kvPairs.map(_.flatten) <^> { case NonEmptyList(h, l) => foldWithPrefixes(h, l.toList: _*) }
    } yield kvPairsProducts.map(js.Dictionary[AnyRef](_: _*))
  }

  override def platformPrelude = JSPrelude

}

