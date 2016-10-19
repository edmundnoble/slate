package qq
package cc

import cats.data.{ValidatedNel, Xor}

import scala.language.higherKinds
import monix.eval.Task
import monix.cats._
import qq.data._
import qq.util._

import scala.collection.immutable.Nil
import cats.implicits._

object QQRuntime {

  import QQRuntimeException._

  val taskOfListOfNull: Task[ValidatedNel[QQRuntimeError, List[JSON]]] = Task.now(List(JSON.Null).validNel)
  val emptyArray: JSON.Arr = JSON.Arr()

  @inline final def makePathComponentGetter(component: PathComponent): CompiledProgram = component match {
    case CollectResults => QQRuntime.collectResults
    case SelectKey(key) => QQRuntime.selectKey(key)
    case SelectIndex(index) => QQRuntime.selectIndex(index)
    case SelectRange(start, end) => QQRuntime.selectRange(start, end)
  }

  def modifyPath(component: PathComponent)(f: CompiledProgram): CompiledProgram = component match {
    case CollectResults => {
      case arr: JSON.Arr => arr.value.traverse(f).map(_.traverseM[ValidatedNel[QQRuntimeError, ?], JSON](identity))
      case v => Task.now(typeError("collect results from", "array" -> v).invalidNel)
    }
    case SelectKey(key) => {
      case obj: JSON.Obj =>
        val asMap = obj.toMap
        asMap.value.get(key).fold(taskOfListOfNull)(f).map(_.map(_.map(v => asMap.copy(value = asMap.value + (key -> v)))))
      case v => Task.now(typeError("select key \"" + key + "\" in", "object" -> v).invalidNel)
    }
    case SelectIndex(index) => {
      case arr: JSON.Arr =>
        if (arr.value.length <= index) {
          taskOfListOfNull
        } else {
          f(arr.value(index)).map {
            _.map(_.map({ v =>
              JSON.Arr(arr.value.updated(index, v))
            }))
          }
        }
      case v =>
        Task.now(typeError(
          "select index " + index + " in", "array" -> v).invalidNel)
    }
    case SelectRange(start, end) => ???
  }

  def setPath(components: List[PathComponent], biggerStructure: JSON, smallerStructure: JSON): Task[ValidatedNel[QQRuntimeError, List[JSON]]] =
    components match {
      case (component :: rest) => component match {
        case CollectResults => biggerStructure match {
          case arr: JSON.Arr => arr.value.traverse(setPath(rest, _, smallerStructure)).map(_.traverseM[ValidatedNel[QQRuntimeError, ?], JSON](identity))
          case v => Task.now(typeError("collect results from", "array" -> v).invalidNel)
        }
        case SelectKey(key) => biggerStructure match {
          case obj: JSON.Obj =>
            val asMap = obj.toMap
            asMap.value.get(key).fold(taskOfListOfNull)(
              setPath(rest, _, smallerStructure).map(_.map(_.map(nv => asMap.copy(value = asMap.value.updated(key, nv)))))
            )
          case v => Task.now(typeError("select key \"" + key + "\" from ", "array" -> v).invalidNel)
        }
        case SelectIndex(index) => biggerStructure match {
          case arr: JSON.Arr =>
            if (arr.value.length <= index) {
              ???
            } else {
              setPath(rest, arr.value(index), smallerStructure).map(_.map(_.map(v => JSON.Arr(arr.value.updated(index, v)))))
            }
          case v =>
            Task.now(typeError("select index " + index + " in", "array" -> v).invalidNel)
        }
        case SelectRange(start, end) => ???
      }
      case Nil => Task.now(List(smallerStructure).validNel)
    }

  def constNumber(num: Double): CompiledFilter =
    CompiledFilter.const(JSON.Num(num))

  def constString(str: String): CompiledFilter =
    CompiledFilter.const(JSON.Str(str))

  def constBoolean(bool: Boolean): CompiledFilter =
    CompiledFilter.const(if (bool) JSON.True else JSON.False)

  def addJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      JSON.Num(f + s).validNel
    case (JSON.Str(f), JSON.Str(s)) =>
      JSON.Str(f + s).validNel
    case (f: JSON.Arr, s: JSON.Arr) =>
      JSON.Arr(f.value ++ s.value).validNel
    case (f: JSON.Obj, s: JSON.Obj) =>
      JSON.ObjMap(f.toMap.value ++ s.toMap.value).validNel
    case (f, s) =>
      typeError(
        "add",
        "number | string | array | object" -> f,
        "number | string | array | object" -> s).invalidNel
  }

  def subtractJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      JSON.Num(f - s).validNel
    case (f: JSON.Arr, s: JSON.Arr) =>
      JSON.Arr(f.value.filter(!s.value.contains(_))).validNel
    case (f: JSON.Obj, s: JSON.Obj) =>
      val contents: Map[String, JSON] = f.toMap.value -- s.map[String, Set[String]](_._1)(collection.breakOut)
      JSON.ObjMap(contents).validNel
    case (f, s) =>
      typeError(
        "subtract",
        "number | array | object" -> f,
        "number | array | object" -> s).invalidNel
  }

  def multiplyJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f * s).validNel
    case (JSON.Str(f), JSON.Num(s)) => (if (s == 0) JSON.Null else JSON.Str(f * s.toInt)).validNel
    case (f: JSON.Obj, s: JSON.Obj) =>
      val firstMapValid = f.toMap.value.mapValues(_.validNel[QQRuntimeError])
      val secondMapValid = s.toMap.value.mapValues(_.validNel[QQRuntimeError])
      Unsafe.mapTraverse[String].sequence[ValidatedNel[QQRuntimeError, ?], JSON](
        qq.util.unionWith(firstMapValid, secondMapValid)(
          (f, s) => (f |@| s).map (addJsValues).flatten
        )
      ).map(JSON.ObjMap)
    case (f, s) =>
      typeError(
        "multiply",
        "number | string | object" -> f,
        "number | object" -> s).invalidNel
  }

  def divideJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f / s).validNel
    case (f, s) =>
      typeError("divide", "number" -> f, "number" -> s).invalidNel
  }

  def moduloJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f % s).validNel
    case (f, s) =>
      typeError("modulo", "number" -> f, "number" -> s).invalidNel
  }

  def equalJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] =
    (if (first == second) JSON.True else JSON.False).validNel

  def lteJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f <= s) JSON.True else JSON.False).validNel
    case (f, s) =>
      typeError("lte", "number" -> f, "number" -> s).invalidNel
  }

  def gteJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f >= s) JSON.True else JSON.False).validNel
    case (f, s) =>
      typeError("gte", "number" -> f, "number" -> s).invalidNel
  }

  def lessThanJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f < s) JSON.True else JSON.False).validNel
    case (f, s) =>
      typeError("lessThan", "number" -> f, "number" -> s).invalidNel
  }

  def greaterThanJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f > s) JSON.True else JSON.False).validNel
    case (f, s) =>
      typeError("greaterThan", "number" -> f, "number" -> s).invalidNel
  }

  def not(v: JSON): ValidatedNel[QQRuntimeError, JSON] = v match {
    case JSON.True => JSON.False.validNel
    case JSON.False => JSON.True.validNel
    case k => typeError("not", "boolean" -> k).invalidNel
  }

  def enlistFilter(filter: CompiledFilter): CompiledFilter =
    (bindings: VarBindings) =>
      (jsv: JSON) =>
        for {
          results <- filter(bindings)(jsv)
        } yield results.map(JSON.Arr(_) :: Nil)

  def selectKey(key: String): CompiledProgram = {
    case f: JSON.Obj =>
      f.toMap.value.get(key) match {
        case None => taskOfListOfNull
        case Some(v) => Task.now((v :: Nil).validNel)
      }
    case v =>
      Task.now(typeError("select key " + key, "object" -> v).invalidNel)
  }

  def selectIndex(index: Int): CompiledProgram = {
    case f: JSON.Arr =>
      val seq = f.value
      if (index >= -seq.length) {
        if (index >= 0 && index < seq.length) {
          Task.now((seq(index) :: Nil).validNel)
        } else if (index < 0) {
          Task.now((seq(seq.length + index) :: Nil).validNel)
        } else {
          taskOfListOfNull
        }
      } else {
        taskOfListOfNull
      }
    case v =>
      Task.now(typeError("select index " + index.toString, "array" -> v).invalidNel)
  }

  def selectRange(start: Int, end: Int): CompiledProgram = {
    case f: JSON.Arr =>
      val seq = f.value
      if (start < end && start < seq.length) {
        Task.now((JSON.Arr(seq.slice(start, end)) :: Nil).validNel)
      } else {
        Task.now((emptyArray :: Nil).validNel)
      }
    case v =>
      Task.now(typeError("select range " + start + ":" + end, "array" -> v).invalidNel)
  }

  def collectResults: CompiledProgram = {
    case arr: JSON.Arr =>
      Task.now(arr.value.validNel)
    case dict: JSON.Obj =>
      Task.now(dict.map[JSON, List[JSON]](_._2)(collection.breakOut).validNel)
    case v =>
      Task.now(typeError("flatten", "array" -> v).invalidNel)
  }

  def enjectFilter(obj: List[(String Xor CompiledFilter, CompiledFilter)]): CompiledFilter = {
    if (obj.isEmpty) {
      CompiledFilter.func(_ => Task.now((JSON.Obj() :: Nil).validNel))
    } else {
      bindings: VarBindings =>
        jsv: JSON =>
          for {
            kvPairs <- obj.traverse[TaskParallel, ValidatedNel[QQRuntimeError, List[List[(String, JSON)]]]] {
              case (Xor.Right(filterKey), filterValue) =>
                (for {
                  keyResults <- filterKey(bindings)(jsv)
                  valueResults <- filterValue(bindings)(jsv)
                  keyValuePairs = keyResults.flatMap {
                    _.traverse[ValidatedNel[QQRuntimeError, ?], List[(String, JSON)]] {
                      case JSON.Str(keyString) =>
                        valueResults.map(_.map(keyString -> _))
                      case k =>
                        typeError("use as key", "string" -> k).invalidNel
                    }
                  }
                } yield keyValuePairs).parallel
              case (Xor.Left(filterName), filterValue) =>
                filterValue(bindings)(jsv).map(_.map(_.map(filterName -> _) :: Nil)).parallel
            }.unwrap
            kvPairsProducts = kvPairs.traverse[ValidatedNel[QQRuntimeError, ?], List[List[(String, JSON)]]](identity).map(_.map(_.flatten).unconsFold(Nil, foldWithPrefixes[(String, JSON)](_, _: _*)))
          } yield kvPairsProducts.map(_.map(JSON.ObjList))
    }
  }

  def printType(value: JSON): String = value match {
    case _: JSON.Num => "number"
    case JSON.True | JSON.False => "boolean"
    case _: JSON.Arr => "array"
    case _: JSON.Obj => "object"
    case JSON.Null => "null"
    case _: JSON.Str => "string"
  }

  def print(value: JSON): String = JSON.render(value)
}
