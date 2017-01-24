package qq.cc

import cats.Applicative
import cats.data.ValidatedNel
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import qq.cc.FlatTraverseArrs._taskPar
import qq.data._
import qq.data.ast._
import qq.util._

object QQInterpreterRuntime extends QQRuntime[InterpretedFilter] {

  import QQRuntimeException._

  val taskOfVectorOfNull: Eff[InterpretedFilterStack, Vector[JSON]] = (JSON.`null` +: Vector.empty[JSON]).pureEff[InterpretedFilterStack]
  val emptyArray: JSON = JSON.Arr()

  final def funFromMathOperator: Map[MathOperator, InterpretedMathOperator] = Map(
    Add -> addJsValues _,
    Subtract -> subtractJsValues _,
    Multiply -> multiplyJsValues _,
    Divide -> divideJsValues _,
    Modulo -> moduloJsValues _,
    Equal -> equalJsValues _,
    LTE -> lteJsValues _,
    GTE -> gteJsValues _,
    LessThan -> lessThanJsValues _,
    GreaterThan -> greaterThanJsValues _
  )

  @inline final def evaluatePath(components: Vector[PathComponent], operation: PathOperationF[InterpretedFilter]): InterpretedFilter = operation match {
    case PathGet =>
      components
        .map(makePathComponentGetter[InterpretedFilterStack])
        .nelFoldLeft1(InterpretedFilter.id)(InterpretedFilter.composeFilters)
    case PathSet(set) =>
      InterpretedFilter.singleton(j =>
        set(j).flatMap(
          _.flatTraverseA(setPath[InterpretedFilterStack](components, j, _))
        )
      )
    case PathModify(modify) =>
      components
        .map(modifyPath[InterpretedFilterStack])
        .nelFoldLeft1(identity[InterpretedFilter])((f, s) => (i: InterpretedFilter) => f(s(i)))(modify)
  }

  final def dereference(name: String): InterpretedFilter = {
    InterpretedFilter.singleton {
      (_: JSON) =>
        for {
          bindings <- reader.ask[InterpretedFilterStack, VarBindings]
          result <- bindings.get(name).fold(noSuchVariable[InterpretedFilterStack, JSON](name))(_.value.pureEff[InterpretedFilterStack])
        } yield result +: Vector.empty[JSON]
    }
  }

  final def filterNot(): InterpretedFilter = InterpretedFilter.singleton { j =>
    Eff.send[OrRuntimeErr, InterpretedFilterStack, Vector[JSON]](not(j).map(_ +: Vector.empty))
  }

  final def compose(first: InterpretedFilter, second: InterpretedFilter): InterpretedFilter =
    InterpretedFilter.composeFilters(first, second)

  def filterMath(first: InterpretedFilter, second: InterpretedFilter, op: MathOperator): InterpretedFilter = {
    InterpretedFilter.singleton { j =>
      (first(j) |@| second(j)).map((v1, v2) =>
        (for {v1a <- v1; v2a <- v2} yield Eff.send[OrRuntimeErr, InterpretedFilterStack, JSON](funFromMathOperator(op)(v1a, v2a))).traverseA(identity)
      ).flatten
    }
  }

  def silenceExceptions(f: InterpretedFilter): InterpretedFilter =
    InterpretedFilter.singleton { (jsv: JSON) =>
      either.catchLeft(f(jsv))((_: RuntimeErrs) => Vector.empty[JSON].pureEff)
    }

  @inline final def makePathComponentGetter[R: _taskPar : _runtimeErr](component: PathComponent): FlatTraverseArrs[R, Vector, JSON, JSON] =
    FlatTraverseArrs.singleton[R, Vector, JSON, JSON] {
      component match {
        case CollectResults => collectResults[R]
        case SelectKey(key) => selectKey[R](key)
        case SelectIndex(index: Int) => selectIndex[R](index)
        case SelectRange(start: Int, end: Int) => selectRange[R](start, end)
      }
    }

  def modifyPath[R: _taskPar : _runtimeErr](component: PathComponent)(f: FlatTraverseArrs[R, Vector, JSON, JSON]): FlatTraverseArrs[R, Vector, JSON, JSON] =
    FlatTraverseArrs.singleton[R, Vector, JSON, JSON] {
      component match {
        case CollectResults => {
          case arr: JSON.Arr => arr.value.traverseA(f(_)).map(_.flatten)
          case v: JSON => typeErrorE[R, Vector[JSON]]("collect results from", "array" -> v)
        }
        case SelectKey(key) => {
          case obj: JSON.Obj =>
            val asMap = obj.toMap
            asMap.value.get(key).fold((JSON.`null` +: Vector.empty[JSON]).pureEff[R])(f(_))
              .map(_.map(v => asMap.copy(value = asMap.value + (key -> v))))
          case v: JSON => typeErrorE[R, Vector[JSON]]("select key \"" + key + "\" in", "object" -> v)
        }
        case SelectIndex(index: Int) => {
          case arr: JSON.Arr =>
            if (arr.value.length <= index) {
              (JSON.`null` +: Vector.empty[JSON]).pureEff[R]
            } else {
              f(arr.value(index)).map(_.map {
                v =>
                  JSON.arr(arr.value.updated(index, v): _*)
              })
            }
          case v: JSON =>
            typeErrorE[R, Vector[JSON]]("select index " + index + " in", "array" -> v)
        }
        case SelectRange(start: Int, end: Int) => ???
      }
    }

  def setPath[R: _taskPar : _runtimeErr](components: Vector[PathComponent], biggerStructure: JSON, smallerStructure: JSON): Eff[R, Vector[JSON]] =
    components match {
      case (component +: rest) => component match {
        case CollectResults => biggerStructure match {
          case arr: JSON.Arr => arr.value.traverseA(setPath[R](rest, _, smallerStructure)).map(_.flatten)
          case v: JSON => typeErrorE[R, Vector[JSON]]("collect results from", "array" -> v)
        }
        case SelectKey(key) => biggerStructure match {
          case obj: JSON.Obj =>
            val asMap = obj.toMap
            asMap.value.get(key).fold((JSON.`null` +: Vector.empty[JSON]).pureEff[R])(
              setPath[R](rest, _, smallerStructure).map(_.map(nv => asMap.copy(value = asMap.value.updated(key, nv)): JSON))
            )
          case v: JSON => typeErrorE[R, Vector[JSON]]("select key \"" + key + "\" in", "object" -> v)
        }
        case SelectIndex(index: Int) => biggerStructure match {
          case arr: JSON.Arr =>
            val filledArr = arr.value ++ Vector.fill(index - (arr.value.length - 1))(JSON.`null`)
            setPath[R](rest, filledArr(index), smallerStructure).map(_.map(v => JSON.Arr(filledArr.updated(index, v))))
          case v: JSON =>
            typeErrorE[R, Vector[JSON]]("select index " + index + " in", "array" -> v)
        }
        case SelectRange(start: Int, end: Int) => ???
      }
      case f if f.isEmpty => (smallerStructure +: Vector.empty[JSON]).pureEff[R]
    }

  def constNumber(num: Double): InterpretedFilter =
    InterpretedFilter.const(JSON.Num(num))

  def constString(str: String): InterpretedFilter =
    InterpretedFilter.const(JSON.Str(str))

  def constBoolean(bool: Boolean): InterpretedFilter =
    InterpretedFilter.const(if (bool) JSON.True else JSON.False)

  def addJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      Either.right(JSON.Num(f + s))
    case (JSON.Str(f), JSON.Str(s)) =>
      Either.right(JSON.Str(f + s))
    case (f: JSON.Arr, s: JSON.Arr) =>
      Either.right(JSON.Arr(f.value ++ s.value))
    case (f: JSON.Obj, s: JSON.Obj) =>
      Either.right(JSON.ObjMap(f.toMap.value ++ s.toMap.value))
    case (f, s) =>
      typeError(
        "add",
        "number | string | array | object" -> f,
        "number | string | array | object" -> s)
  }

  def subtractJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      Either.right(JSON.Num(f - s))
    case (f: JSON.Arr, s: JSON.Arr) =>
      Either.right(JSON.Arr(f.value.filter(!s.value.contains(_))))
    case (f: JSON.Obj, s: JSON.Obj) =>
      val contents: Map[String, JSON] = f.toMap.value -- s.map[String, Set[String]](_._1)(collection.breakOut)
      Either.right(JSON.ObjMap(contents))
    case (f, s) =>
      typeError(
        "subtract",
        "number | array | object" -> f,
        "number | array | object" -> s)
  }

  def multiplyJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => Either.right(JSON.Num(f * s))
    case (JSON.Str(f), JSON.Num(s)) => Either.right(if (s == 0) JSON.Null else JSON.Str(f * s.toInt))
    case (f: JSON.Obj, s: JSON.Obj) =>
      val firstMapValid = f.toMap.value.mapValues(_.valid[RuntimeErrs])
      val secondMapValid = s.toMap.value.mapValues(_.valid[RuntimeErrs])
      Unsafe.mapTraverse[String].sequence[ValidatedNel[QQRuntimeError, ?], JSON](
        qq.util.unionWith(firstMapValid, secondMapValid)(
          Applicative[ValidatedNel[QQRuntimeError, ?]].map2(_, _)(addJsValues(_, _).toValidated).flatten
        )
      ).map(JSON.ObjMap).toEither
    case (f, s) =>
      typeError(
        "multiply",
        "number | string | object" -> f,
        "number | object" -> s)
  }

  def divideJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => Either.right(JSON.Num(f / s))
    case (f, s) =>
      typeError("divide", "number" -> f, "number" -> s)
  }

  def moduloJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) => Either.right(JSON.Num(f % s))
    case (f, s) =>
      typeError("modulo", "number" -> f, "number" -> s)
  }

  def equalJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] =
    Either.right(if (first == second) JSON.True else JSON.False)

  def lteJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      Either.right(if (f <= s) JSON.True else JSON.False)
    case (f, s) =>
      typeError("lte", "number" -> f, "number" -> s)
  }

  def gteJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      Either.right(if (f >= s) JSON.True else JSON.False)
    case (f, s) =>
      typeError("gte", "number" -> f, "number" -> s)
  }

  def lessThanJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      Either.right(if (f < s) JSON.True else JSON.False)
    case (f, s) =>
      typeError("lessThan", "number" -> f, "number" -> s)
  }

  def greaterThanJsValues(first: JSON, second: JSON): OrRuntimeErr[JSON] = (first, second) match {
    case (JSON.Num(f), JSON.Num(s)) =>
      Either.right(if (f > s) JSON.True else JSON.False)
    case (f, s) =>
      typeError("greaterThan", "number" -> f, "number" -> s)
  }

  def not(v: JSON): OrRuntimeErr[JSON] = v match {
    case JSON.True => Either.right(JSON.False)
    case JSON.False => Either.right(JSON.True)
    case k: JSON => typeError("not", "boolean" -> k)
  }

  def enlistFilter(filter: InterpretedFilter): InterpretedFilter =
    InterpretedFilter.singleton(j =>
      filter(j).map {
        JSON.Arr(_) +: Vector.empty[JSON]
      }
    )

  def selectKey[R: _taskPar : _runtimeErr](key: String): JSON => Eff[R, Vector[JSON]] = {
    case f: JSON.Obj =>
      f.toMap.value.get(key) match {
        case None => (JSON.`null` +: Vector.empty[JSON]).pureEff[R]
        case Some(v: JSON) => (v +: Vector.empty[JSON]).pureEff[R]
      }
    case v: JSON =>
      typeErrorE[R, Vector[JSON]]("select key " + key, "object" -> v)
  }

  def selectIndex[R: _runtimeErr](index: Int): JSON => Eff[R, Vector[JSON]] = {
    case f: JSON.Arr =>
      val seq = f.value
      ((if (index >= -seq.length) {
        if (index >= 0 && index < seq.length) {
          seq(index)
        } else if (index < 0) {
          seq(seq.length + index)
        } else {
          JSON.`null`
        }
      } else {
        JSON.`null`
      }) +: Vector.empty[JSON]).pureEff[R]
    case v: JSON =>
      typeErrorE[R, Vector[JSON]]("select index " + index.toString, "array" -> v)
  }

  def selectRange[R: _taskPar : _runtimeErr](start: Int, end: Int): JSON => Eff[R, Vector[JSON]] = {
    case f: JSON.Arr =>
      val seq = f.value
      if (start < end && start < seq.length) {
        (JSON.arr(seq.slice(start, end): _*) +: Vector.empty[JSON]).pureEff[R]
      } else {
        (emptyArray +: Vector.empty[JSON]).pureEff[R]
      }
    case v: JSON =>
      typeErrorE[R, Vector[JSON]]("select range " + start + ":" + end, "array" -> v)
  }

  def collectResults[R: _taskPar : _runtimeErr]: JSON => Eff[R, Vector[JSON]] = {
    case arr: JSON.Arr =>
      arr.value.pureEff
    case dict: JSON.Obj =>
      dict.map[JSON, Vector[JSON]](_._2)(collection.breakOut).pureEff
    case v: JSON =>
      typeErrorE[R, Vector[JSON]]("flatten", "array" -> v)
  }

  def asBinding(name: String, as: InterpretedFilter, in: InterpretedFilter): InterpretedFilter = InterpretedFilter.singleton { (j: JSON) =>
    for {
      bindings <- eff.reader.ask[InterpretedFilterStack, VarBindings]
      results <- as(j)
      inRan <- results.traverseA(r => eff.reader.runReader(bindings + (name -> VarBinding(name, r)))(in(j))).into[InterpretedFilterStack]
    } yield inRan.flatten
  }

  def enjectFilter(obj: Vector[(String Either InterpretedFilter, InterpretedFilter)]): InterpretedFilter = {
    if (obj.isEmpty) {
      InterpretedFilter.singleton(_ => (JSON.obj() +: Vector.empty[JSON]).pureEff)
    } else {
      InterpretedFilter.singleton(
        (jsv: JSON) =>
          obj.traverseA[InterpretedFilterStack, Vector[(String, JSON)]] {
            case (Right(filterKey), filterValue) =>
              for {
                keyResults <- filterKey(jsv)
                valueResults <- filterValue(jsv)
                keyValuePairs <- keyResults.traverseA {
                  case JSON.Str(keyString) => valueResults.map(keyString -> _).pureEff[InterpretedFilterStack]
                  case k => typeErrorE[InterpretedFilterStack, Vector[(String, JSON)]]("use as key", "string" -> k)
                }
              } yield keyValuePairs.flatten
            case (Left(filterName), filterValue) =>
              for {
                valueResult <- filterValue(jsv)
              } yield valueResult.map(filterName -> _)
          }.map(_.unconsFold(Vector.empty, foldWithPrefixesV).map(JSON.ObjList))
      )
    }
  }

  def ensequence(first: InterpretedFilter, second: InterpretedFilter): InterpretedFilter =
    InterpretedFilter.singleton(j =>
      (first(j) |@| second(j)).map(_ ++ _)
    )

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
