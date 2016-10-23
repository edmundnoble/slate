package qq
package cc

import cats.data.{NonEmptyList, ValidatedNel, Xor}

import scala.language.higherKinds
import monix.eval.Task
import monix.cats._
import qq.data._
import qq.util._

import scala.collection.immutable.Nil
import cats.implicits._
import org.atnos.eff._
import Eff._
import syntax.all._

object QQRuntime {

  import QQRuntimeException._

  val taskOfListOfNull: CompiledFilterResult[List[JSON]] = (JSON.`null` :: Nil).pureEff[CompiledFilterStack]
  val emptyArray: JSON = JSON.Arr()

  @inline final def makePathComponentGetter(component: PathComponent): CompiledProgram = component match {
    case CollectResults => QQRuntime.collectResults
    case SelectKey(key) => QQRuntime.selectKey(key)
    case SelectIndex(index: Int) => QQRuntime.selectIndex(index)
    case SelectRange(start: Int, end: Int) => QQRuntime.selectRange(start, end)
  }

  def modifyPath(component: PathComponent)(f: CompiledProgram): CompiledProgram = CompiledProgram.singleton(
    component match {
      case CollectResults => {
        case arr: JSON.Arr => arr.value.traverseA(f(_)).map(_.flatten)
        case v: JSON => typeError("collect results from", "array" -> v)
      }
      case SelectKey(key) => {
        case obj: JSON.Obj =>
          val asMap = obj.toMap
          asMap.value.get(key).fold((JSON.`null` :: Nil).pureEff[CompiledProgramStack])(f(_))
            .map(_.map(v => asMap.copy(value = asMap.value + (key -> v))))
        case v: JSON => typeError("select key \"" + key + "\" in", "object" -> v)
      }
      case SelectIndex(index: Int) => {
        case arr: JSON.Arr =>
          if (arr.value.length <= index) {
            (JSON.`null` :: Nil).pureEff[CompiledProgramStack]
          } else {
            f(arr.value(index)).map(_.map { v =>
              JSON.arr(arr.value.updated(index, v): _*)
            })
          }
        case v: JSON =>
          typeError("select index " + index + " in", "array" -> v)
      }
      case SelectRange(start: Int, end: Int) => ???
    }
  )

  type SetPathStack = Fx.fx2[OrRuntimeErr, Task]
  type SetPathResult[A] = Eff[SetPathStack, A]

  def setPath(components: List[PathComponent], biggerStructure: JSON, smallerStructure: JSON): Eff[SetPathStack, List[JSON]] =
    components match {
      case (component :: rest) => component match {
        case CollectResults => biggerStructure match {
          case arr: JSON.Arr => arr.value.traverseA[SetPathStack, List[JSON]](setPath(rest, _, smallerStructure)).map(_.flatten)
          case v: JSON => typeError[SetPathStack, List[JSON]]("collect results from", "array" -> v)
        }
        case SelectKey(key) => biggerStructure match {
          case obj: JSON.Obj =>
            val asMap = obj.toMap
            println(asMap)
            asMap.value.get(key).fold((JSON.`null` :: Nil).pureEff[SetPathStack])(
              setPath(rest, _, smallerStructure).map(_.map(nv => asMap.copy(value = asMap.value.updated(key, nv)): JSON))
            )
          case v: JSON => typeError[SetPathStack, List[JSON]]("select key \"" + key + "\" from ", "array" -> v)
        }
        case SelectIndex(index: Int) => biggerStructure match {
          case arr: JSON.Arr =>
            if (arr.value.length <= index) {
              ???
            } else {
              setPath(rest, arr.value(index), smallerStructure).map(_.map(v => JSON.Arr(arr.value.updated(index, v))))
            }
          case v: JSON =>
            typeError[SetPathStack, List[JSON]]("select index " + index + " in", "array" -> v)
        }
        case SelectRange(start: Int, end: Int) => ???
      }
      case Nil => (smallerStructure :: Nil).pureEff[SetPathStack]
    }

  def constNumber(num: Double): CompiledFilter =
    CompiledFilter.const(JSON.Num(num))

  def constString(str: String): CompiledFilter =
    CompiledFilter.const(JSON.Str(str))

  def constBoolean(bool: Boolean): CompiledFilter =
    CompiledFilter.const(if (bool) JSON.True else JSON.False)

  def addJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = Tuple2(first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      JSON.Num(f + s).valid
    case (JSON.Str(f), JSON.Str(s)) =>
      JSON.Str(f + s).valid
    case (f: JSON.Arr, s: JSON.Arr) =>
      JSON.Arr(f.value ++ s.value).valid
    case (f: JSON.Obj, s: JSON.Obj) =>
      JSON.ObjMap(f.toMap.value ++ s.toMap.value).valid
    case (f, s) =>
      (TypeError(
        "add",
        "number | string | array | object" -> f,
        "number | string | array | object" -> s): QQRuntimeError).invalidNel
  }

  def subtractJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = Tuple2(first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      JSON.Num(f - s).valid
    case (f: JSON.Arr, s: JSON.Arr) =>
      JSON.Arr(f.value.filter(!s.value.contains(_))).valid
    case (f: JSON.Obj, s: JSON.Obj) =>
      val contents: Map[String, JSON] = f.toMap.value -- s.map[String, Set[String]](_._1)(collection.breakOut)
      JSON.ObjMap(contents).valid
    case (f, s) =>
      (TypeError(
        "subtract",
        "number | array | object" -> f,
        "number | array | object" -> s): QQRuntimeError).invalidNel
  }

  def multiplyJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = Tuple2(first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f * s).valid
    case (JSON.Str(f), JSON.Num(s)) => (if (s == 0) JSON.Null else JSON.Str(f * s.toInt)).valid
    case (f: JSON.Obj, s: JSON.Obj) =>
      val firstMapValid = f.toMap.value.mapValues(_.valid[NonEmptyList[QQRuntimeError]])
      val secondMapValid = s.toMap.value.mapValues(_.valid[NonEmptyList[QQRuntimeError]])
      Unsafe.mapTraverse[String].sequence[OrRuntimeErr, JSON](
        qq.util.unionWith(firstMapValid, secondMapValid)(
          (f, s) => (f |@| s).map(addJsValues).flatten
        )
      ).map(JSON.ObjMap)
    case (f, s) =>
      (TypeError(
        "multiply",
        "number | string | object" -> f,
        "number | object" -> s): QQRuntimeError).invalidNel
  }

  def divideJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = Tuple2(first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f / s).valid
    case (f, s) =>
      (TypeError("divide", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def moduloJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = Tuple2(first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f % s).valid
    case (f, s) =>
      (TypeError("modulo", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def equalJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] =
    (if (first == second) JSON.True else JSON.False).valid

  def lteJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = Tuple2(first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f <= s) JSON.True else JSON.False).valid
    case (f, s) =>
      (TypeError("lte", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def gteJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = Tuple2(first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f >= s) JSON.True else JSON.False).valid
    case (f, s) =>
      (TypeError("gte", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def lessThanJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = Tuple2(first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f < s) JSON.True else JSON.False).valid
    case (f, s) =>
      (TypeError("lessThan", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def greaterThanJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = Tuple2(first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f > s) JSON.True else JSON.False).valid
    case (f, s) =>
      (TypeError("greaterThan", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def not(v: JSON): OrRuntimeErr[JSON] = v match {
    case JSON.True => JSON.False.valid
    case JSON.False => JSON.True.valid
    case k: JSON => (TypeError("not", "boolean" -> k): QQRuntimeError).invalidNel
  }

  def enlistFilter(filter: CompiledFilter): CompiledFilter =
    CompiledFilter.singleton(j =>
      filter(j).map {
        JSON.Arr(_) :: Nil
      }
    )

  def selectKey(key: String): CompiledProgram = CompiledProgram.singleton {
    case f: JSON.Obj =>
      f.toMap.value.get(key) match {
        case None => EffMonad[CompiledProgramStack].pure(JSON.`null` :: Nil)
        case Some(v: JSON) => EffMonad[CompiledProgramStack].pure(v :: Nil)
      }
    case v: JSON =>
      typeError[CompiledProgramStack, List[JSON]]("select key " + key, "object" -> v)
  }

  def selectIndex(index: Int): CompiledProgram = CompiledProgram.singleton {
    case f: JSON.Arr =>
      val seq = f.value
      if (index >= -seq.length) {
        if (index >= 0 && index < seq.length) {
          (seq(index) :: Nil).pureEff[CompiledProgramStack]
        } else if (index < 0) {
          (seq(seq.length + index) :: Nil).pureEff[CompiledProgramStack]
        } else {
          (JSON.`null` :: Nil).pureEff[CompiledProgramStack]
        }
      } else {
        (JSON.`null` :: Nil).pureEff[CompiledProgramStack]
      }
    case v: JSON =>
      typeError[CompiledProgramStack, List[JSON]]("select index " + index.toString, "array" -> v)
  }

  def selectRange(start: Int, end: Int): CompiledProgram = CompiledProgram.singleton {
    case f: JSON.Arr =>
      val seq = f.value
      if (start < end && start < seq.length) {
        (JSON.arr(seq.slice(start, end): _*) :: Nil).pureEff[CompiledProgramStack]
      } else {
        (emptyArray :: Nil).pureEff[CompiledProgramStack]
      }
    case v: JSON =>
      typeError[CompiledProgramStack, List[JSON]]("select range " + start + ":" + end, "array" -> v)
  }

  def collectResults: CompiledProgram = CompiledProgram.singleton {
    case arr: JSON.Arr =>
      arr.value.pureEff
    case dict: JSON.Obj =>
      dict.map[JSON, List[JSON]](_._2)(collection.breakOut).pureEff
    case v: JSON =>
      typeError[CompiledProgramStack, List[JSON]]("flatten", "array" -> v)
  }

  def enjectFilter(obj: List[(String Xor CompiledFilter, CompiledFilter)]): CompiledFilter = {
    if (obj.isEmpty) {
      CompiledFilter.singleton(_ => (JSON.obj() :: Nil).pureEff)
    } else {
      CompiledFilter.singleton(
        (jsv: JSON) =>
          obj.traverseA[CompiledFilterStack, List[(String, JSON)]] {
            case (Xor.Right(filterKey), filterValue) =>
              for {
                keyResults <- filterKey(jsv)
                valueResults <- filterValue(jsv)
                keyValuePairs <- keyResults.traverseA {
                  case JSON.Str(keyString) => valueResults.map(keyString -> _).pureEff[CompiledFilterStack]
                  case k => typeError[CompiledFilterStack, List[(String, JSON)]]("use as key", "string" -> k)
                }
              } yield keyValuePairs.flatten
            case (Xor.Left(filterName), filterValue) =>
              for {
                valueResult <- filterValue(jsv)
              } yield valueResult.map(filterName -> _)
          }.map(_.unconsFold(Nil, foldWithPrefixes(_, _: _*)).map(JSON.ObjList(_)))
      )
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
