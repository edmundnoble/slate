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
import org.atnos.eff._, Eff._, syntax.all._

object QQRuntime {

  import QQRuntimeException._

  val taskOfListOfNull: CompiledFilterResult[JSON] = EffMonad[CompiledFilterStack].pure[JSON](JSON.Null)
  val emptyArray: JSON.Arr = JSON.Arr()

  @inline final def makePathComponentGetter(component: PathComponent): CompiledProgram = component match {
    case CollectResults => QQRuntime.collectResults
    case SelectKey(key) => QQRuntime.selectKey(key)
    case SelectIndex(index) => QQRuntime.selectIndex(index)
    case SelectRange(start, end) => QQRuntime.selectRange(start, end)
  }

  def modifyPath(component: PathComponent)(f: CompiledProgram): CompiledProgram = component match {
    case CollectResults => {
      case arr: JSON.Arr => arr.value.traverse[CompiledFilterResult, JSON](f(_))
      case v => Task.now(typeError("collect results from", "array" -> v).invalid)
    }
    case SelectKey(key) => {
      case obj: JSON.Obj =>
        val asMap = obj.toMap
        asMap.value.get(key).fold(taskOfListOfNull)(f(_)).map(v => asMap.copy(value = asMap.value + (key -> v)))
      case v => Task.now(typeError("select key \"" + key + "\" in", "object" -> v).invalid)
    }
    case SelectIndex(index) => {
      case arr: JSON.Arr =>
        if (arr.value.length <= index) {
          taskOfListOfNull
        } else {
          f(arr.value(index)).map { v =>
            JSON.Arr(arr.value.updated(index, v))
          }
        }
      case v =>
        Task.now(typeError(
          "select index " + index + " in", "array" -> v).invalid)
    }
    case SelectRange(start, end) => ???
  }

  type SetPathStack = Fx.fx3[ValidatedNel[QQRuntimeError, ?], Task, List]
  type SetPathResult[A] = Eff[SetPathStack, A]

  def setPath(components: List[PathComponent], biggerStructure: JSON, smallerStructure: JSON): Eff[SetPathStack, JSON] =
    components match {
      case (component :: rest) => component match {
        case CollectResults => biggerStructure match {
          case arr: JSON.Arr => arr.value.traverse(setPath(rest, _, smallerStructure))
          case v => typeError("collect results from", "array" -> v)
        }
        case SelectKey(key) => biggerStructure match {
          case obj: JSON.Obj =>
            val asMap = obj.toMap
            asMap.value.get(key).fold(taskOfListOfNull)(
              setPath(rest, _, smallerStructure).map(_.map(_.map(nv => asMap.copy(value = asMap.value.updated(key, nv)))))
            )
          case v => Task.now(typeError("select key \"" + key + "\" from ", "array" -> v).invalid)
        }
        case SelectIndex(index) => biggerStructure match {
          case arr: JSON.Arr =>
            if (arr.value.length <= index) {
              ???
            } else {
              setPath(rest, arr.value(index), smallerStructure).map(_.map(_.map(v => JSON.Arr(arr.value.updated(index, v)))))
            }
          case v =>
            Task.now(typeError("select index " + index + " in", "array" -> v).invalid)
        }
        case SelectRange(start, end) => ???
      }
      case Nil => Task.now(List(smallerStructure).valid)
    }

  def constNumber(num: Double): CompiledFilter =
    CompiledFilter.const(JSON.Num(num))

  def constString(str: String): CompiledFilter =
    CompiledFilter.const(JSON.Str(str))

  def constBoolean(bool: Boolean): CompiledFilter =
    CompiledFilter.const(if (bool) JSON.True else JSON.False)

  def addJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      JSON.Num(f + s).valid
    case (JSON.Str(f), JSON.Str(s)) =>
      JSON.Str(f + s).valid
    case (f: JSON.Arr, s: JSON.Arr) =>
      JSON.Arr(f.value ++ s.value).valid
    case (f: JSON.Obj, s: JSON.Obj) =>
      JSON.ObjMap(f.toMap.value ++ s.toMap.value).valid
    case (f, s) =>
      typeError(
        "add",
        "number | string | array | object" -> f,
        "number | string | array | object" -> s).invalid
  }

  def subtractJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      JSON.Num(f - s).valid
    case (f: JSON.Arr, s: JSON.Arr) =>
      JSON.Arr(f.value.filter(!s.value.contains(_))).valid
    case (f: JSON.Obj, s: JSON.Obj) =>
      val contents: Map[String, JSON] = f.toMap.value -- s.map[String, Set[String]](_._1)(collection.breakOut)
      JSON.ObjMap(contents).valid
    case (f, s) =>
      typeError(
        "subtract",
        "number | array | object" -> f,
        "number | array | object" -> s).invalid
  }

  def multiplyJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f * s).valid
    case (JSON.Str(f), JSON.Num(s)) => (if (s == 0) JSON.Null else JSON.Str(f * s.toInt)).valid
    case (f: JSON.Obj, s: JSON.Obj) =>
      val firstMapValid = f.toMap.value.mapValues(_.valid[QQRuntimeError])
      val secondMapValid = s.toMap.value.mapValues(_.valid[QQRuntimeError])
      Unsafe.mapTraverse[String].sequence[ValidatedNel[QQRuntimeError, ?], JSON](
        qq.util.unionWith(firstMapValid, secondMapValid)(
          (f, s) => (f |@| s).map(addJsValues).flatten
        )
      ).map(JSON.ObjMap)
    case (f, s) =>
      typeError(
        "multiply",
        "number | string | object" -> f,
        "number | object" -> s).invalid
  }

  def divideJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f / s).valid
    case (f, s) =>
      typeError("divide", "number" -> f, "number" -> s).invalid
  }

  def moduloJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f % s).valid
    case (f, s) =>
      typeError("modulo", "number" -> f, "number" -> s).invalid
  }

  def equalJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] =
    (if (first == second) JSON.True else JSON.False).valid

  def lteJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f <= s) JSON.True else JSON.False).valid
    case (f, s) =>
      typeError("lte", "number" -> f, "number" -> s).invalid
  }

  def gteJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f >= s) JSON.True else JSON.False).valid
    case (f, s) =>
      typeError("gte", "number" -> f, "number" -> s).invalid
  }

  def lessThanJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f < s) JSON.True else JSON.False).valid
    case (f, s) =>
      typeError("lessThan", "number" -> f, "number" -> s).invalid
  }

  def greaterThanJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f > s) JSON.True else JSON.False).valid
    case (f, s) =>
      typeError("greaterThan", "number" -> f, "number" -> s).invalid
  }

  def not(v: JSON): ValidatedNel[QQRuntimeError, JSON] = v match {
    case JSON.True => JSON.False.valid
    case JSON.False => JSON.True.valid
    case k => typeError("not", "boolean" -> k).invalid
  }

  def enlistFilter(filter: CompiledFilter): CompiledFilter =
    CompiledFilter.singleton(j => list.runList(filter(j)).map(JSON.Arr(_)))
//      (jsv: JSON) =>
//        for {
//          results <- filter(bindings)(jsv)
//        } yield results.map(JSON.Arr(_) :: Nil)

  def selectKey(key: String): CompiledProgram = {
    case f: JSON.Obj =>
      f.toMap.value.get(key) match {
        case None => taskOfListOfNull
        case Some(v) => EffMonad[CompiledFilterResult].pure(v)
      }
    case v =>
      Task.now(typeError("select key " + key, "object" -> v).invalid)
  }

  def selectIndex(index: Int): CompiledProgram = {
    case f: JSON.Arr =>
      val seq = f.value
      if (index >= -seq.length) {
        if (index >= 0 && index < seq.length) {
          Task.now((seq(index) :: Nil).valid)
        } else if (index < 0) {
          Task.now((seq(seq.length + index) :: Nil).valid)
        } else {
          taskOfListOfNull
        }
      } else {
        taskOfListOfNull
      }
    case v =>
      Task.now(typeError("select index " + index.toString, "array" -> v).invalid)
  }

  def selectRange(start: Int, end: Int): CompiledProgram = {
    case f: JSON.Arr =>
      val seq = f.value
      if (start < end && start < seq.length) {
        Task.now((JSON.Arr(seq.slice(start, end)) :: Nil).valid)
      } else {
        Task.now((emptyArray :: Nil).valid)
      }
    case v =>
      Task.now(typeError("select range " + start + ":" + end, "array" -> v).invalid)
  }

  def collectResults: CompiledProgram = {
    case arr: JSON.Arr =>
      Task.now(arr.value.valid)
    case dict: JSON.Obj =>
      Task.now(dict.map[JSON, List[JSON]](_._2)(collection.breakOut).valid)
    case v =>
      Task.now(typeError("flatten", "array" -> v).invalid)
  }

  def enjectFilter(obj: List[(String Xor CompiledFilter, CompiledFilter)]): CompiledFilter = {
    if (obj.isEmpty) {
      CompiledFilter.func(_ => Task.now((JSON.Obj() :: Nil).valid))
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
                        typeError("use as key", "string" -> k).invalid
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
