package qq
package cc
package jsc

import monix.eval.Task
import monix.scalaz._
import data._
import util._

import scala.scalajs.js
import scalaz.std.list._
import scalaz.std.map._
import scalaz.syntax.std.map._
import scalaz.syntax.traverse._
import scalaz.{-\/, Reader, \/, \/-}
import JsUtil._
import Platform.Js._

// This is a QQ runtime which executes all operations on native JSON values in Javascript
object JSRuntime extends QQRuntime[Any] {

  val taskOfListOfNull: Task[List[Any]] = Task.now(List(null))
  val emptyArray: js.Array[Any] = new js.Array[Any](0)

  override def modifyPath(component: PathComponent)(f: CompiledProgram[Any]): CompiledProgram[Any] = component match {
    case CollectResults => {
      case arr: js.Array[Any@unchecked] => arr.toList.traverseM(f)
      case v => Task.raiseError(QQRuntimeException("Tried to collect results from " + print(v) + " but it's not an array"))
    }
    case SelectKey(key) => {
      case obj: js.Object => obj
        .toDictionary.toList
        .traverse[Task, List[(String, Any)]] {
        case (k, v) =>
          if (k == key) f(v).map(_.map(k -> _))
          else Task.now((k -> v) :: Nil)
      }
        .map(_.map(_.toJSDictionary))
      case v => Task.raiseError(QQRuntimeException("Tried to select key " + key + " from " + print(v) + " but it's not an array"))
    }
    case SelectIndex(index) => {
      case arr: js.Array[Any@unchecked] =>
        if (arr.length <= index) {
          Task.raiseError(???)
        } else {
          f(arr(index)).map {
            _.map { v =>
              val newArr = arr.jsSlice(0, arr.length)
              newArr(index) = v
              newArr
            }
          }
        }
      case v =>
        Task.raiseError(QQRuntimeException("Tried to select index " + index + " from " + print(v) + " but it's not an array"))
    }
    case SelectRange(start, end) => ???
  }

  override def setPath(components: List[PathComponent], biggerStructure: Any, smallerStructure: Any): Task[List[Any]] = components match {
    case (component :: rest) => component match {
      case CollectResults => biggerStructure match {
        case arr: js.Array[Any@unchecked] => arr.toList.traverseM(setPath(rest, _, smallerStructure))
        case v => Task.raiseError(QQRuntimeException("Tried to collect results from " + print(v) + " but it's not an array"))
      }
      case SelectKey(key) => biggerStructure match {
        case obj: js.Object =>
          obj.toDictionary.get(key).fold(Task.now((null: Any) :: Nil))(v =>
            setPath(rest, v, smallerStructure).map(_.map(nv => js.Dictionary(obj.toDictionary.updated(key, nv).toList: _*)))
          )
        case v => Task.raiseError(QQRuntimeException("Tried to select key " + key + " from " + print(v) + " but it's not an array"))
      }
      case SelectIndex(index) => biggerStructure match {
        case arr: js.Array[Any@unchecked] =>
          if (arr.length <= index) {
            Task.raiseError(???)
          } else {
            setPath(rest, arr(index), smallerStructure).map(_.map(v => arr.updated(index, v)))
          }
        case v =>
          Task.raiseError(QQRuntimeException("Tried to select index " + index + " from " + print(v) + " but it's not an array"))
      }
      case SelectRange(start, end) => ???
    }
    case Nil => Task.now(List(smallerStructure))
  }

  override def constNumber(num: Double): CompiledFilter[Any] = CompiledFilter.const(num)

  override def constString(str: String): CompiledFilter[Any] = CompiledFilter.const(str)

  override def addJsValues(first: Any, second: Any): Task[Any] = (first, second) match {
    case (f: Double, s: Double) =>
      Task.now(f + s)
    case (f: String, s: String) =>
      Task.now(f + s)
    case (f: js.Array[Any@unchecked], s: js.Array[Any@unchecked]) =>
      Task.now(f.concat(s))
    case (f: js.Object, s: js.Object) =>
      val firstMap = f.toDictionary.toMap
      val secondMap = s.toDictionary.toMap
      Task.now(js.Dictionary((firstMap ++ secondMap).toSeq: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't add " + print(f) + " and " + print(s)))
  }

  override def subtractJsValues(first: Any, second: Any): Task[Any] = (first, second) match {
    case (f: Double, s: Double) =>
      Task.now(f - s)
    case (f: js.Array[Any@unchecked], s: js.Array[Any@unchecked]) =>
      Task.now(f.filter(!s.contains(_)))
    case (f: js.Object, s: js.Object) =>
      val contents: Seq[(String, Any)] = (f.asInstanceOf[js.Dictionary[Any]].toMap -- s.asInstanceOf[js.Dictionary[Any]].toMap.keySet).toSeq
      Task.now(js.Dictionary(contents: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't subtract " + print(f) + " and " + print(s)))
  }

  override def multiplyJsValues(first: Any, second: Any): Task[Any] = (first, second) match {
    case (f: Double, s: Double) =>
      Task.now(f * s)
    case (f: String, s: Int) =>
      Task.now(if (s < 0) null else f * s)
    case (f: js.Object, s: js.Object) =>
      val firstMap = f.toDictionary.toMap.mapValues(Task.now)
      val secondMap = s.toDictionary.toMap.mapValues(Task.now)
      mapInstance[String].sequence(firstMap.unionWith(secondMap) {
        Task.mapBoth(_, _)(addJsValues).flatten
      }).map(o => js.Dictionary(o.toSeq: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't multiply " + print(f) + " and " + print(s)))
  }

  override def divideJsValues(first: Any, second: Any): Task[Any] = (first, second) match {
    case (f: Double, s: Double) =>
      Task.now(f / s)
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't divide " + print(f) + " by " + print(s)))
  }

  override def moduloJsValues(first: Any, second: Any): Task[Any] = (first, second) match {
    case (f: Double, s: Double) =>
      Task.now(f % s)
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't modulo " + print(f) + " by " + print(s)))
  }

  override def enlistFilter(filter: CompiledFilter[Any]): CompiledFilter[Any] = (for {
    fFun <- Reader(filter)
  } yield {
    jsv: Any =>
      for {
        results <- fFun(jsv)
      } yield js.Array(results: _*) :: Nil
  }).run

  override def selectKey(key: String): CompiledProgram[Any] = {
    case f: js.Object =>
      f.toDictionary.get(key) match {
        case None => taskOfListOfNull
        case Some(v) => Task.now(v :: Nil)
      }
    case v =>
      Task.raiseError(QQRuntimeException("Tried to select key " + key + " in " + print(v) + " but it's not a dictionary"))
  }

  override def selectIndex(index: Int): CompiledProgram[Any] = {
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
      Task.raiseError(QQRuntimeException("Tried to select index " + print(index) + " in " + print(v) + " but it's not an array"))
  }

  override def selectRange(start: Int, end: Int): CompiledProgram[Any] = {
    case f: js.Array[js.Object@unchecked] =>
      if (start < end && start < f.length) {
        Task.now(f.jsSlice(start, end) :: Nil)
      } else {
        Task.now(emptyArray :: Nil)
      }
  }

  override def collectResults: CompiledProgram[Any] = {
    case arr: js.Array[Any@unchecked] =>
      Task.now(arr.toList)
    case dict: js.Object =>
      Task.now(dict.toDictionary.values.toList)
    case v =>
      Task.raiseError(QQRuntimeException("Tried to flatten " + print(v) + " but it's not an array"))
  }

  override def enjectFilter(obj: List[(\/[String, CompiledFilter[Any]], CompiledFilter[Any])]): CompiledFilter[Any] = {
    if (obj.isEmpty) {
      CompiledFilter.const[Any](js.Object())
    } else {
      bindings: VarBindings[Any] =>
        jsv: Any =>
          for {
            kvPairs <- obj.traverse[Task, List[List[(String, Any)]]] {
              case (\/-(filterKey), filterValue) =>
                for {
                  keyResults <- filterKey(bindings)(jsv)
                  valueResults <- filterValue(bindings)(jsv)
                  keyValuePairs <- keyResults.traverse[Task, List[(String, Any)]] {
                    case (keyString: String) =>
                      Task.now(valueResults.map(keyString -> _))
                    case k =>
                      Task.raiseError(QQRuntimeException("Tried to use " + print(k) + " as a key for an object but it's not a string"))
                  }
                } yield keyValuePairs
              case (-\/(filterName), filterValue) =>
                filterValue(bindings)(jsv).map(_.map(filterName -> _) :: Nil)
            }
            kvPairsProducts = kvPairs.map(_.flatten).unconsFold(Nil, foldWithPrefixes[(String, Any)](_, _: _*))
          } yield kvPairsProducts.map(js.Dictionary[Any](_: _ *))
    }
  }

  override def platformPrelude: PlatformPrelude[Any] = JSPrelude

  override def print(value: Any): String = Json.jsToString(value)

}

