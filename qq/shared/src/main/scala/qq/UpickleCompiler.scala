package qq

import upickle.Js
import monix.eval.Task
import qq.Compiler.{CompiledFilter, QQRuntimeException}

import scalaz.{-\/, NonEmptyList, \/, \/-}
import scalaz.std.list._
import scalaz.syntax.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.applicative._
import qq.Util._
import com.thoughtworks.each.Monadic._

import scalaz.syntax.std.map._
import scalaz.std.map._
import scala.collection.immutable.Nil

object UpickleCompiler extends Compiler {
  override type AnyTy = Js.Value

  val taskOfListOfNull: Task[List[AnyTy]] = Task.now(List(Js.Null))
  val emptyArray: Js.Arr = Js.Arr()

  override def constNumber(num: Double): CompiledFilter[this.type] = {
    _ => Task.now(Js.Num(num) :: Nil)
  }

  override def constString(str: String): CompiledFilter[this.type] = {
    _ => Task.now(Js.Str(str) :: Nil)
  }

  def addJsValues(first: Js.Value, second: Js.Value): Task[Js.Value] = (first, second) match {
    case (Js.Num(f), Js.Num(s)) =>
      Task.now(Js.Num(f + s))
    case (Js.Str(f), Js.Str(s)) =>
      Task.now(Js.Str(f + s))
    case (f: Js.Arr, s: Js.Arr) =>
      Task.now(Js.Arr(f.value ++ s.value: _*))
    case (f: Js.Obj, s: Js.Obj) =>
      Task.now(Js.Obj((f.value.toMap ++ s.value.toMap).toSeq: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException(s"can't add $f and $s"))
  }

  def subtractJsValues(first: Js.Value, second: Js.Value): Task[Js.Value] = (first, second) match {
    case (Js.Num(f), Js.Num(s)) =>
      Task.now(Js.Num(f - s))
    case (f: Js.Arr, s: Js.Arr) =>
      Task.now(Js.Arr(f.value.filter(!s.value.contains(_)): _*))
    case (f: Js.Obj, s: Js.Obj) =>
      val contents: Seq[(String, Js.Value)] = (f.value.toMap -- s.value.map[String, Set[String]](_._1)(collection.breakOut)).toSeq
      Task.now(Js.Obj(contents: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException(s"can't subtract $f and $s"))
  }

  def multiplyJsValues(first: Js.Value, second: Js.Value): Task[Js.Value] = (first, second) match {
    case (Js.Num(f), Js.Num(s)) => Task.now(f * s)
      Task.now(if (s == 0) Js.Null else Js.Num(f * s))
    case (Js.Str(f), Js.Num(s)) => Task.now(Js.Str(f + s))
    case (f: Js.Obj, s: Js.Obj) =>
      val firstMap = f.value.toMap.mapValues(Task.now)
      val secondMap = s.value.toMap.mapValues(Task.now)
      firstMap.unionWith(secondMap) { (f, s) => Task.mapBoth(f, s)(addJsValues).flatten[Js.Value] }.sequence.map(o => Js.Obj(o.toSeq: _*))
    case (f, s) =>
      Task.raiseError(QQRuntimeException(s"can't multiply $f and $s"))
  }

  def divideJsValues(first: Js.Value, second: Js.Value): Task[Js.Value] = (first, second) match {
    case (Js.Num(f), Js.Num(s)) => Task.now(Js.Num(f / s))
    case (f, s) =>
      Task.raiseError(QQRuntimeException(s"can't divide $f by $s"))
  }

  def moduloJsValues(first: Js.Value, second: Js.Value): Task[Js.Value] = (first, second) match {
    case (Js.Num(f), Js.Num(s)) => Task.now(Js.Num(f % s))
    case (f, s) =>
      Task.raiseError(QQRuntimeException(s"can't modulo $f by $s"))
  }

  def enlistFilter(filter: CompiledFilter[this.type]): CompiledFilter[this.type] = { jsv: Js.Value =>
    for {
      results <- filter(jsv)
    } yield Js.Arr(results: _*) :: Nil
  }

  def selectKey(key: String): CompiledFilter[this.type] = {
    case f: Js.Obj =>
      f.value.find(_._1 == key) match {
        case None => taskOfListOfNull
        case Some((_, v)) => Task.now(v :: Nil)
      }
    case v =>
      Task.raiseError(QQRuntimeException(s"Tried to select key $key in $v but it's not a dictionary"))
  }

  def selectIndex(index: Int): CompiledFilter[this.type] = {
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
      Task.raiseError(QQRuntimeException(s"Tried to select index $index in $v but it's not an array"))
  }

  def selectRange(start: Int, end: Int): CompiledFilter[this.type] = {
    case f: Js.Arr =>
      val seq = f.value
      if (start < end && start < seq.length) {
        Task.now(Js.Arr(seq.slice(start, end): _*) :: Nil)
      } else {
        Task.now(emptyArray :: Nil)
      }
    case v =>
      Task.raiseError(QQRuntimeException(s"Tried to select range $start:$end in $v but it's not an array"))
  }

  def collectResults(f: CompiledFilter[this.type]): CompiledFilter[this.type] = { jsv: Js.Value =>
    f(jsv).flatMap {
      _.traverseM {
        case arr: Js.Arr =>
          Task.now(arr.value.toList)
        case dict: Js.Obj =>
          Task.now(dict.value.map(_._2)(collection.breakOut))
        case v =>
          Task.raiseError(QQRuntimeException(s"Tried to flatten $v but it's not an array"))
      }
    }
  }

  override def enjectFilter(obj: List[(\/[String, CompiledFilter[this.type]], CompiledFilter[this.type])]): CompiledFilter[this.type] = { jsv: AnyTy =>
    for {
      kvPairs <- obj.traverse {
        case (\/-(filterKey), filterValue) =>
          val res = for {
            keyResults <- filterKey(jsv)
            valueResults <- filterValue(jsv)
            keyValuePairs <- keyResults.traverse {
              case Js.Str(keyString) =>
                Task.now(valueResults.map(keyString -> _))
              case k =>
                Task.raiseError(QQRuntimeException(s"Tried to use $k as a key for an object but it's not a string"))
            }
          } yield keyValuePairs
          res
        case (-\/(filterName), filterValue) =>
          filterValue(jsv).map(_.map(filterName -> _) :: Nil)
      }
      kvPairsProducts = kvPairs.map(_.flatten) <^> { case NonEmptyList(h, l) => l.foldLeft(h :: Nil)(withPrefixes) }
    } yield kvPairsProducts.map(Js.Obj(_: _*))
  }

  def platformPrelude = UpicklePrelude

}
