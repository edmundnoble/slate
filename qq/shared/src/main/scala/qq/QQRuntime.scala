package qq

import monix.eval.Task
import qq.FilterComponent._

import scalaz.\/

trait QQRuntime[JsonTy] {

  import QQCompiler._

  def print(value: JsonTy): String

  def platformPrelude: PlatformPrelude[JsonTy]

  def enjectFilter(obj: List[(\/[String, CompiledFilter[JsonTy]], CompiledFilter[JsonTy])]): CompiledFilter[JsonTy]

  def enlistFilter(filter: CompiledFilter[JsonTy]): CompiledFilter[JsonTy]

  @inline final def evaluateLeaf(component: LeafComponent[JsonTy]): CompiledFilter[JsonTy] = component match {
    case IdFilter() => (jsv: JsonTy) => Task.now(jsv :: Nil)
    case ConstNumber(num) => constNumber(num)
    case ConstString(str) => constString(str)
    case SelectKey(key) => selectKey(key)
    case SelectIndex(index) => selectIndex(index)
    case SelectRange(start, end) => selectRange(start, end)
  }

  def selectKey(key: String): CompiledFilter[JsonTy]

  def selectIndex(index: Int): CompiledFilter[JsonTy]

  def selectRange(start: Int, end: Int): CompiledFilter[JsonTy]

  def constNumber(num: Double): CompiledFilter[JsonTy]

  def constString(str: String): CompiledFilter[JsonTy]

  def addJsValues(first: JsonTy, second: JsonTy): Task[JsonTy]

  def subtractJsValues(first: JsonTy, second: JsonTy): Task[JsonTy]

  def multiplyJsValues(first: JsonTy, second: JsonTy): Task[JsonTy]

  def divideJsValues(first: JsonTy, second: JsonTy): Task[JsonTy]

  def moduloJsValues(first: JsonTy, second: JsonTy): Task[JsonTy]

  def collectResults(f: CompiledFilter[JsonTy]): CompiledFilter[JsonTy]

}
