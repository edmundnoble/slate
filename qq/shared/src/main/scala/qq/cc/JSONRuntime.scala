package qq
package cc

import monix.eval.Task
import monix.scalaz._
import qq.data._
import qq.util._

import scala.collection.immutable.Nil
import scalaz.std.list._
import scalaz.std.map._
import scalaz.syntax.tag._
import scalaz.syntax.traverse._
import scalaz.{-\/, \/, \/-}

object JSONRuntime extends QQRuntime[JSON] {

  val taskOfListOfNull: Task[List[JSON]] = Task.now(List(JSON.Null))
  val emptyArray: JSON.Arr = JSON.Arr()

  override def modifyPath(component: PathComponent)(f: CompiledProgram[JSON]): CompiledProgram[JSON] = component match {
    case CollectResults => {
      case arr: JSON.Arr => arr.value.traverseM(f)
      case v => Task.raiseError(QQRuntimeException("Tried to collect results from " + print(v) + " but it's not an array"))
    }
    case SelectKey(key) => {
      case obj: JSON.Obj =>
        val asMap = obj.toMap
        asMap.value.get(key).fold(Task.now((JSON.Null: JSON) :: Nil))(f).map(_.map(v => asMap.copy(value = asMap.value + (key -> v))))
      case v => Task.raiseError(QQRuntimeException("Tried to select key " + key + " from " + print(v) + " but it's not an array"))
    }
    case SelectIndex(index) => {
      case arr: JSON.Arr =>
        if (arr.value.length <= index) {
          Task.now(JSON.Null :: Nil)
        } else {
          f(arr.value(index)).map {
            _.map { v =>
              JSON.Arr(arr.value.updated(index, v))
            }
          }
        }
      case v =>
        Task.raiseError(QQRuntimeException("Tried to select index " + index + " from " + print(v) + " but it's not an array"))
    }
    case SelectRange(start, end) => ???
  }

  override def setPath(components: List[PathComponent], biggerStructure: JSON, smallerStructure: JSON): Task[List[JSON]] = components match {
    case (component :: rest) => component match {
      case CollectResults => biggerStructure match {
        case arr: JSON.Arr => arr.value.traverseM(setPath(rest, _, smallerStructure))
        case v => Task.raiseError(QQRuntimeException("Tried to collect results from " + print(v) + " but it's not an array"))
      }
      case SelectKey(key) => biggerStructure match {
        case obj: JSON.Obj =>
          val asMap = obj.toMap
          asMap.value.get(key).fold(Task.now((JSON.Null: JSON) :: Nil))(
            setPath(rest, _, smallerStructure).map(_.map(nv => asMap.copy(value = asMap.value.updated(key, nv))))
          )
        case v => Task.raiseError(QQRuntimeException("Tried to select key " + key + " from " + print(v) + " but it's not an array"))
      }
      case SelectIndex(index) => biggerStructure match {
        case arr: JSON.Arr =>
          if (arr.value.length <= index) {
            Task.raiseError(???)
          } else {
            setPath(rest, arr.value(index), smallerStructure).map(_.map(v => JSON.Arr(arr.value.updated(index, v))))
          }
        case v =>
          Task.raiseError(QQRuntimeException("Tried to select index " + index + " from " + print(v) + " but it's not an array"))
      }
      case SelectRange(start, end) => ???
    }
    case Nil => Task.now(List(smallerStructure))
  }

  override def constNumber(num: Double): CompiledFilter[JSON] =
    CompiledFilter.const(JSON.Num(num))

  override def constString(str: String): CompiledFilter[JSON] =
    CompiledFilter.const(JSON.Str(str))

  override def constBoolean(bool: Boolean): CompiledFilter[JSON] =
    CompiledFilter.const(if (bool) JSON.True else JSON.False)

  override def addJsValues(first: JSON, second: JSON): Task[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      Task.now(JSON.Num(f + s))
    case (JSON.Str(f), JSON.Str(s)) =>
      Task.now(JSON.Str(f + s))
    case (f: JSON.Arr, s: JSON.Arr) =>
      Task.now(JSON.Arr(f.value ++ s.value))
    case (f: JSON.Obj, s: JSON.Obj) =>
      Task.now(JSON.ObjMap(f.toMap.value ++ s.toMap.value))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't add " + print(f) + " and " + print(s)))
  }

  override def subtractJsValues(first: JSON, second: JSON): Task[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      Task.now(JSON.Num(f - s))
    case (f: JSON.Arr, s: JSON.Arr) =>
      Task.now(JSON.Arr(f.value.filter(!s.value.contains(_))))
    case (f: JSON.Obj, s: JSON.Obj) =>
      val contents: Map[String, JSON] = f.toMap.value -- s.map[String, Set[String]](_._1)(collection.breakOut)
      Task.now(JSON.ObjMap(contents))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't subtract " + print(f) + " and " + print(s)))
  }

  override def multiplyJsValues(first: JSON, second: JSON): Task[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => Task.now(f * s)
      Task.now(if (s == 0) JSON.Null else JSON.Num(f * s))
    case (JSON.Str(f), JSON.Num(s)) => Task.now(JSON.Str(f + s))
    case (f: JSON.Obj, s: JSON.Obj) =>
      val firstMapTask = f.toMap.value.mapValues(Task.now(_).parallel)
      val secondMapTask = s.toMap.value.mapValues(Task.now(_).parallel)
      mapInstance[String].sequence[TaskParallel, JSON](
        unionWith(firstMapTask, secondMapTask)(
          (f, s) => Task.mapBoth(f.unwrap, s.unwrap)(addJsValues).flatten.parallel
        )
      ).unwrap.map(JSON.ObjMap)
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't multiply " + print(f) + " and " + print(s)))
  }

  override def divideJsValues(first: JSON, second: JSON): Task[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => Task.now(JSON.Num(f / s))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't divide " + print(f) + " by " + print(s)))
  }

  override def moduloJsValues(first: JSON, second: JSON): Task[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => Task.now(JSON.Num(f % s))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't modulo " + print(f) + " by " + print(s)))
  }

  def equalJsValues(first: JSON, second: JSON): Task[JSON] =
    Task.now(if (first == second) JSON.True else JSON.False)

  override def enlistFilter(filter: CompiledFilter[JSON]): CompiledFilter[JSON] =
    (bindings: VarBindings[JSON]) =>
      (jsv: JSON) =>
        for {
          results <- filter(bindings)(jsv)
        } yield JSON.Arr(results) :: Nil

  override def selectKey(key: String): CompiledProgram[JSON] = {
    case f: JSON.Obj =>
      f.toMap.value.get(key) match {
        case None => taskOfListOfNull
        case Some(v) => Task.now(v :: Nil)
      }
    case v =>
      Task.raiseError(QQRuntimeException("Tried to select key " + key + " in " + print(v) + " but it's not a dictionary"))
  }

  override def selectIndex(index: Int): CompiledProgram[JSON] = {
    case f: JSON.Arr =>
      val seq = f.value
      if (index >= -seq.length) {
        if (index >= 0 && index < seq.length) {
          Task.now(seq(index) :: Nil)
        } else if (index < 0) {
          Task.now(seq(seq.length + index) :: Nil)
        } else {
          taskOfListOfNull
        }
      } else {
        taskOfListOfNull
      }
    case v =>
      Task.raiseError(QQRuntimeException("Tried to select index " + index.toString + " in " + print(v) + " but it's not an array"))
  }

  override def selectRange(start: Int, end: Int): CompiledProgram[JSON] = {
    case f: JSON.Arr =>
      val seq = f.value
      if (start < end && start < seq.length) {
        Task.now(JSON.Arr(seq.slice(start, end)) :: Nil)
      } else {
        Task.now(emptyArray :: Nil)
      }
    case v =>
      Task.raiseError(QQRuntimeException("Tried to select range " +
        start + ":" + end + " in " + print(v) +
        " but it's not an array"))
  }

  override def collectResults: CompiledProgram[JSON] = {
    case arr: JSON.Arr =>
      Task.now(arr.value)
    case dict: JSON.Obj =>
      Task.now(dict.map(_._2)(collection.breakOut))
    case v =>
      Task.raiseError(QQRuntimeException("Tried to flatten " + print(v) + " but it's not an array"))
  }

  override def enjectFilter(obj: List[(\/[String, CompiledFilter[JSON]], CompiledFilter[JSON])]): CompiledFilter[JSON] = {
    if (obj.isEmpty) {
      CompiledFilter.func[JSON](_ => Task.now(JSON.Obj() :: Nil))
    } else {
      bindings: VarBindings[JSON] => {
        jsv: JSON =>
          for {
            kvPairs <- obj.traverse[TaskParallel, List[List[(String, JSON)]]] {
              case (\/-(filterKey), filterValue) =>
                (for {
                  keyResults <- filterKey(bindings)(jsv)
                  valueResults <- filterValue(bindings)(jsv)
                  keyValuePairs <- keyResults.traverse[Task, List[(String, JSON)]] {
                    case JSON.Str(keyString) =>
                      Task.now(valueResults.map(keyString -> _))
                    case k =>
                      Task.raiseError(QQRuntimeException("Tried to use " + print(k) + " as a key for an object but it's not a string"))
                  }
                } yield keyValuePairs).parallel
              case (-\/(filterName), filterValue) =>
                filterValue(bindings)(jsv).map(_.map(filterName -> _) :: Nil).parallel
            }.unwrap
            kvPairsProducts = kvPairs.map(_.flatten).unconsFold(Nil, foldWithPrefixes[(String, JSON)](_, _: _*))
          } yield kvPairsProducts.map(JSON.ObjList)
      }
    }
  }

  override def platformPrelude: PlatformPrelude[JSON] = JSONPrelude

  override def print(value: JSON): String = JSON.render(value)

}
