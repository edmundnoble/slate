package qq

import upickle.Js
import monix.eval.Task
import qq.QQCompiler.QQRuntimeException

import scalaz.{-\/, \/, \/-}
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.applicative._
import qq.Util._

object QQUpickleCompiler extends QQCompiler {
  override type AnyTy = Js.Value

  val taskOfListOfNull: Task[List[AnyTy]] = Task.now(List(Js.Null))
  val emptyArray: Js.Arr = Js.Arr()

  def enlistFilter(filter: CompiledFilter): CompiledFilter = { jsv: Js.Value =>
    for {
      results <- filter(jsv)
    } yield Js.Arr(results: _*) :: Nil
  }

  def selectKey(key: String): CompiledFilter = {
    case f: Js.Obj =>
      f.value.find(_._1 == key) match {
        case None => taskOfListOfNull
        case Some((_, v)) => Task.now(v :: Nil)
      }
    case v =>
      Task.raiseError(new QQRuntimeException(s"Tried to select key $key in $v but it's not a dictionary"))
  }

  def selectIndex(index: Int): CompiledFilter = {
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
      Task.raiseError(new QQRuntimeException(s"Tried to select index $index in $v but it's not an array"))
  }

  def selectRange(start: Int, end: Int): CompiledFilter = {
    case f: Js.Arr =>
      val seq = f.value
      if (start < end && start < seq.length) {
        Task.now(Js.Arr(seq.slice(start, end): _*) :: Nil)
      } else {
        Task.now(emptyArray :: Nil)
      }
    case v =>
      Task.raiseError(new QQRuntimeException(s"Tried to select range $start:$end in $v but it's not an array"))
  }

  def collectResults(f: CompiledFilter): CompiledFilter = {
    case arr: Js.Arr =>
      Task.now(arr.value.toList)
    case dict: Js.Obj =>
      Task.now(dict.value.map(_._2)(collection.breakOut))
    case v =>
      Task.raiseError(new QQRuntimeException(s"Tried to flatten $v but it's not an array"))
  }

  override def enjectFilter(obj: List[(\/[String, CompiledFilter], CompiledFilter)]): CompiledFilter = { jsv: AnyTy =>
    val kvPairs: Task[List[List[(String, AnyTy)]]] = obj.traverseM {
      case (\/-(filterKey), filterValue) =>
        for {
          keyResults <- filterKey(jsv)
          valueResults <- filterValue(jsv)
          keyValuePairs <- keyResults.traverse {
            case Js.Str(keyString) =>
              Task.now(valueResults.map(keyString -> _))
            case k =>
              Task.raiseError(new QQRuntimeException(s"Tried to use $k as a key for an object but it's not a string"))
          }
        } yield keyValuePairs
      case (-\/(filterName), filterValue) =>
        for {
          valueResults <- filterValue(jsv)
        } yield valueResults.map(filterName -> _) :: Nil
    }
    kvPairs.map(_.map(Js.Obj(_: _*)))
  }
}
