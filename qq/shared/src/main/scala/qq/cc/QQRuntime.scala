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

  val taskOfListOfNull: CompiledFilterResult[JSON] = EffMonad[CompiledFilterStack].pure(JSON.Null)
  val emptyArray: JSON = JSON.Arr()

  @inline final def makePathComponentGetter(component: PathComponent): CompiledProgram = component match {
    case CollectResults => QQRuntime.collectResults
    case SelectKey(key) => QQRuntime.selectKey(key)
    case SelectIndex(index) => QQRuntime.selectIndex(index)
    case SelectRange(start, end) => QQRuntime.selectRange(start, end)
  }

  def modifyPath(component: PathComponent)(f: CompiledProgram): CompiledProgram = CompiledProgram.singleton(component match {
    case CollectResults => {
      case arr: JSON.Arr => Eff.collapse(arr.value.traverse[CompiledProgramResult, JSON](f(_)))
      case v => typeError("collect results from", "array" -> v)
    }
    case SelectKey(key) => {
      case obj: JSON.Obj =>
        val asMap = obj.toMap
        asMap.value.get(key).fold((JSON.Null: JSON).pureEff[CompiledProgramStack])(f(_)).map(v => asMap.copy(value = asMap.value + (key -> v)))
      case v => typeError("select key \"" + key + "\" in", "object" -> v)
    }
    case SelectIndex(index) => {
      case arr: JSON.Arr =>
        if (arr.value.length <= index) {
          (JSON.Null: JSON).pureEff[CompiledProgramStack]
        } else {
          f(arr.value(index)).map { v =>
            JSON.Arr(arr.value.updated(index, v))
          }
        }
      case v =>
        typeError("select index " + index + " in", "array" -> v)
    }
    case SelectRange(start, end) => ???
  })

  type SetPathStack = Fx.fx3[ValidatedNel[QQRuntimeError, ?], Task, List]
  type SetPathResult[A] = Eff[SetPathStack, A]

  def setPath(components: List[PathComponent], biggerStructure: JSON, smallerStructure: JSON): Eff[SetPathStack, JSON] =
    components match {
      case (component :: rest) => component match {
        case CollectResults => biggerStructure match {
          case arr: JSON.Arr => Eff.collapse[SetPathStack, List, JSON](arr.value.traverse[Eff[SetPathStack, ?], JSON](setPath(rest, _, smallerStructure)))
          case v => typeError[SetPathStack, JSON]("collect results from", "array" -> v)
        }
        case SelectKey(key) => biggerStructure match {
          case obj: JSON.Obj =>
            val asMap = obj.toMap
            asMap.value.get(key).fold((JSON.Null: JSON).pureEff[SetPathStack])(
              setPath(rest, _, smallerStructure).map(nv => (asMap.copy(value = asMap.value.updated(key, nv)): JSON))
            )
          case v => typeError[SetPathStack, JSON]("select key \"" + key + "\" from ", "array" -> v)
        }
        case SelectIndex(index) => biggerStructure match {
          case arr: JSON.Arr =>
            if (arr.value.length <= index) {
              ???
            } else {
              setPath(rest, arr.value(index), smallerStructure).map(v => JSON.Arr(arr.value.updated(index, v)))
            }
          case v =>
            typeError[SetPathStack, JSON]("select index " + index + " in", "array" -> v)
        }
        case SelectRange(start, end) => ???
      }
      case Nil => smallerStructure.pureEff[SetPathStack]
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
      (TypeError(
        "add",
        "number | string | array | object" -> f,
        "number | string | array | object" -> s): QQRuntimeError).invalidNel
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
      (TypeError(
        "subtract",
        "number | array | object" -> f,
        "number | array | object" -> s): QQRuntimeError).invalidNel
  }

  def multiplyJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f * s).valid
    case (JSON.Str(f), JSON.Num(s)) => (if (s == 0) JSON.Null else JSON.Str(f * s.toInt)).valid
    case (f: JSON.Obj, s: JSON.Obj) =>
      val firstMapValid = f.toMap.value.mapValues(_.valid[NonEmptyList[QQRuntimeError]])
      val secondMapValid = s.toMap.value.mapValues(_.valid[NonEmptyList[QQRuntimeError]])
      Unsafe.mapTraverse[String].sequence[ValidatedNel[QQRuntimeError, ?], JSON](
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

  def divideJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f / s).valid
    case (f, s) =>
      (TypeError("divide", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def moduloJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => JSON.Num(f % s).valid
    case (f, s) =>
      (TypeError("modulo", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def equalJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] =
    (if (first == second) JSON.True else JSON.False).valid

  def lteJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f <= s) JSON.True else JSON.False).valid
    case (f, s) =>
      (TypeError("lte", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def gteJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f >= s) JSON.True else JSON.False).valid
    case (f, s) =>
      (TypeError("gte", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def lessThanJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f < s) JSON.True else JSON.False).valid
    case (f, s) =>
      (TypeError("lessThan", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def greaterThanJsValues(first: JSON, second: JSON): ValidatedNel[QQRuntimeError, JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      (if (f > s) JSON.True else JSON.False).valid
    case (f, s) =>
      (TypeError("greaterThan", "number" -> f, "number" -> s): QQRuntimeError).invalidNel
  }

  def not(v: JSON): ValidatedNel[QQRuntimeError, JSON] = v match {
    case JSON.True => JSON.False.valid
    case JSON.False => JSON.True.valid
    case k => (TypeError("not", "boolean" -> k): QQRuntimeError).invalidNel
  }

  def enlistFilter(filter: CompiledFilter): CompiledFilter =
    CompiledFilter.singleton(j =>
      list.runList(filter(j)).map(JSON.Arr(_): JSON).into[CompiledFilterStack]
    )
  //      (jsv: JSON) =>
  //        for {
  //          results <- filter(bindings)(jsv)
  //        } yield results.map(JSON.Arr(_) :: Nil)

  def selectKey(key: String): CompiledProgram = CompiledProgram.singleton {
    case f: JSON.Obj =>
      f.toMap.value.get(key) match {
        case None => EffMonad[CompiledProgramStack].pure[JSON](JSON.Null)
        case Some(v) => EffMonad[CompiledProgramStack].pure(v)
      }
    case v =>
      typeError("select key " + key, "object" -> v)
  }

  def selectIndex(index: Int): CompiledProgram = CompiledProgram.singleton {
    case f: JSON.Arr =>
      val seq = f.value
      if (index >= -seq.length) {
        if (index >= 0 && index < seq.length) {
          seq(index).pureEff[CompiledProgramStack]
        } else if (index < 0) {
          seq(seq.length + index).pureEff[CompiledProgramStack]
        } else {
          (JSON.Null: JSON).pureEff[CompiledProgramStack]
        }
      } else {
        (JSON.Null: JSON).pureEff[CompiledProgramStack]
      }
    case v =>
      typeError("select index " + index.toString, "array" -> v)
  }

  def selectRange(start: Int, end: Int): CompiledProgram = CompiledProgram.singleton {
    case f: JSON.Arr =>
      val seq = f.value
      if (start < end && start < seq.length) {
        (JSON.Arr(seq.slice(start, end)): JSON).pureEff[CompiledProgramStack]
      } else {
        emptyArray.pureEff[CompiledProgramStack]
      }
    case v =>
      typeError("select range " + start + ":" + end, "array" -> v)
  }

  def collectResults: CompiledProgram = CompiledProgram.singleton {
    case arr: JSON.Arr =>
      list.fromList(arr.value)
    case dict: JSON.Obj =>
      list.fromList(dict.map(_._2)(collection.breakOut))
    case v =>
      typeError[CompiledProgramStack, JSON]("flatten", "array" -> v)
  }

  def enjectFilter(obj: List[(String Xor CompiledFilter, CompiledFilter)]): CompiledFilter = {
    if (obj.isEmpty) {
      CompiledFilter.singleton(_ => (JSON.Obj(): JSON).pureEff)
    } else {
      CompiledFilter.singleton(
        (jsv: JSON) =>
          obj.traverse[Eff[CompiledFilterStack, ?], (String, JSON)] {
            case (Xor.Right(filterKey), filterValue) =>
              for {
                keyResults <- filterKey(jsv)
                valueResults <- filterValue(jsv)
                keyValuePairs <- keyResults match {
                  case JSON.Str(keyString) => (keyString -> valueResults).pureEff[CompiledFilterStack]
                  case k => typeError[CompiledFilterStack, (String, JSON)]("use as key", "string" -> k)
                }
              } yield keyValuePairs
            case (Xor.Left(filterName), filterValue) =>
              for {
                valueResult <- filterValue(jsv)
              } yield filterName -> valueResult
          }.map(JSON.ObjList(_))
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
