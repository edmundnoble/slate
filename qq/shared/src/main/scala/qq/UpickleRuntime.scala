package qq

import monix.eval.Task
import monix.scalaz._
import qq.FilterComponent._
import upickle.Js
import qq.QQCompiler.{CompiledFilter, FOut, VarBinding}

import scalaz.{-\/, NonEmptyList, Reader, \/, \/-}
import scalaz.std.list._
import scalaz.syntax.std.list._
import scalaz.syntax.traverse._
import qq.Util._

import scalaz.syntax.std.map._
import scalaz.std.map._
import scala.collection.immutable.Nil

object UpickleRuntime extends QQRuntime[Js.Value] {

  val taskOfListOfNull: Task[List[Js.Value]] = Task.now(List(Js.Null))
  val emptyArray: Js.Arr = Js.Arr()

  override def constNumber(num: Double): CompiledFilter[Js.Value] =
    CompiledFilter.const(Js.Num(num))

  override def constString(str: String): CompiledFilter[Js.Value] =
    CompiledFilter.const(Js.Str(str))

  override def addJsValues(first: Js.Value, second: Js.Value): Task[Js.Value] = (first, second) match {
    case (Js.Num(f), Js.Num(s)) =>
      Task.now(Js.Num(f + s))
    case (Js.Str(f), Js.Str(s)) =>
      Task.now(Js.Str(f + s))
    case (f: Js.Arr, s: Js.Arr) =>
      Task.now(Js.Arr(f.value ++ s.value: _*))
    case (f: Js.Obj, s: Js.Obj) =>
      Task.now(Js.Obj((f.value.toMap ++ s.value.toMap).toSeq: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't add " + f.toString + " and " + s.toString))
  }

  override def subtractJsValues(first: Js.Value, second: Js.Value): Task[Js.Value] = (first, second) match {
    case (Js.Num(f), Js.Num(s)) =>
      Task.now(Js.Num(f - s))
    case (f: Js.Arr, s: Js.Arr) =>
      Task.now(Js.Arr(f.value.filter(!s.value.contains(_)): _*))
    case (f: Js.Obj, s: Js.Obj) =>
      val contents: Map[String, Js.Value] = f.value.toMap -- s.value.map[String, Set[String]](_._1)(collection.breakOut)
      Task.now(Js.Obj(contents.toSeq: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't subtract " + f.toString + " and " + s.toString))
  }

  override def multiplyJsValues(first: Js.Value, second: Js.Value): Task[Js.Value] = (first, second) match {
    case (Js.Num(f), Js.Num(s)) => Task.now(f * s)
      Task.now(if (s == 0) Js.Null else Js.Num(f * s))
    case (Js.Str(f), Js.Num(s)) => Task.now(Js.Str(f + s))
    case (f: Js.Obj, s: Js.Obj) =>
      val firstMap = f.value.toMap.mapValues(Task.now)
      val secondMap = s.value.toMap.mapValues(Task.now)
      firstMap.unionWith(secondMap) { (f, s) => Task.mapBoth(f, s)(addJsValues).flatten[Js.Value] }.sequence.map(o => Js.Obj(o.toSeq: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't multiply " + f.toString + " and " + s.toString))
  }

  override def divideJsValues(first: Js.Value, second: Js.Value): Task[Js.Value] = (first, second) match {
    case (Js.Num(f), Js.Num(s)) => Task.now(Js.Num(f / s))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't divide " + f.toString + " by " + s.toString))
  }

  override def moduloJsValues(first: Js.Value, second: Js.Value): Task[Js.Value] = (first, second) match {
    case (Js.Num(f), Js.Num(s)) => Task.now(Js.Num(f % s))
    case (f, s) =>
      Task.raiseError(QQRuntimeException("can't modulo " + f.toString + " by " + s.toString))
  }

  override def enlistFilter(filter: CompiledFilter[Js.Value]): CompiledFilter[Js.Value] = (for {fFun <- Reader(filter)} yield { jsv: Js.Value =>
    for {
      results <- fFun(jsv)
    } yield Js.Arr(results: _*) :: Nil
  }).run

  override def selectKey(key: String): CompiledFilter[Js.Value] = CompiledFilter.func {
    case f: Js.Obj =>
      f.value.find(_._1 == key) match {
        case None => taskOfListOfNull
        case Some((_, v)) => Task.now(v :: Nil)
      }
    case v =>
      Task.raiseError(QQRuntimeException("Tried to select key " + key.toString + " in " + v.toString + " but it's not a dictionary"))
  }

  override def selectIndex(index: Int): CompiledFilter[Js.Value] = CompiledFilter.func {
    case f: Js.Arr =>
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
      Task.raiseError(QQRuntimeException("Tried to select index " + index.toString + " in " + v.toString + " but it's not an array"))
  }

  override def selectRange(start: Int, end: Int): CompiledFilter[Js.Value] = CompiledFilter.func {
    case f: Js.Arr =>
      val seq = f.value
      if (start < end && start < seq.length) {
        Task.now(Js.Arr(seq.slice(start, end): _*) :: Nil)
      } else {
        Task.now(emptyArray :: Nil)
      }
    case v =>
      Task.raiseError(QQRuntimeException("Tried to select range " +
        start.toString + ":" + end.toString + " in " + v.toString +
        " but it's not an array"))
  }

  override def collectResults(f: CompiledFilter[Js.Value]): CompiledFilter[Js.Value] = (for {fFun <- Reader(f)} yield { jsv: Js.Value =>
    fFun(jsv).flatMap {
      _.traverseM {
        case arr: Js.Arr =>
          Task.now(arr.value.toList)
        case dict: Js.Obj =>
          Task.now(dict.value.map(_._2).toList)
        case v =>
          Task.raiseError(QQRuntimeException("Tried to flatten " + v.toString + " but it's not an array"))
      }
    }
  }).run

  override def enjectFilter(obj: List[(\/[String, CompiledFilter[Js.Value]], CompiledFilter[Js.Value])]): CompiledFilter[Js.Value] = {
    if (obj.isEmpty) {
      CompiledFilter.func[Js.Value](_ => Task.now(Js.Obj() :: Nil))
    } else { bindings: List[VarBinding[Js.Value]] => { jsv: Js.Value => for {
      kvPairs <- obj.traverse[Task, List[List[(String, Js.Value)]]] {
        case (\/-(filterKey), filterValue) =>
          for {
            keyResults <- filterKey(bindings)(jsv)
            valueResults <- filterValue(bindings)(jsv)
            keyValuePairs <- keyResults.traverse[Task, List[(String, Js.Value)]] {
              case Js.Str(keyString) =>
                Task.now(valueResults.map(keyString -> _))
              case k =>
                Task.raiseError(QQRuntimeException("Tried to use " + k.toString + " as a key for an object but it's not a string"))
            }
          } yield keyValuePairs
        case (-\/(filterName), filterValue) =>
          filterValue(bindings)(jsv).map(_.map(filterName -> _) :: Nil)
      }
      kvPairsProducts = kvPairs.map(_.flatten) <^> { case NonEmptyList(h, l) => foldWithPrefixes(h, l.toList: _*) }
    } yield kvPairsProducts.map(Js.Obj(_: _ *))
    }
    }
  }

  override def platformPrelude = UpicklePrelude

  override def print(value: Js.Value) = value.toString

}
