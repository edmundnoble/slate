package qq

import monix.eval.Task
import qq.FilterComponent._

import scalaz.\/

trait QQRuntime[AnyTy] {

  import QQCompiler._

  def platformPrelude: PlatformPrelude[AnyTy]

  def enjectFilter(obj: List[(\/[String, CompiledFilter[AnyTy]], CompiledFilter[AnyTy])]): CompiledFilter[AnyTy]

  def enlistFilter(filter: CompiledFilter[AnyTy]): CompiledFilter[AnyTy]

  @inline final def evaluateLeaf(component: LeafComponent[AnyTy]): CompiledFilter[AnyTy] = component match {
    case IdFilter() => (jsv: AnyTy) => Task.now(jsv :: Nil)
    case ConstNumber(num) => constNumber(num)
    case ConstString(str) => constString(str)
    case SelectKey(key) => selectKey(key)
    case SelectIndex(index) => selectIndex(index)
    case SelectRange(start, end) => selectRange(start, end)
  }

  def selectKey(key: String): CompiledFilter[AnyTy]

  def selectIndex(index: Int): CompiledFilter[AnyTy]

  def selectRange(start: Int, end: Int): CompiledFilter[AnyTy]

  def constNumber(num: Double): CompiledFilter[AnyTy]

  def constString(str: String): CompiledFilter[AnyTy]

  def addJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]

  def subtractJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]

  def multiplyJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]

  def divideJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]

  def moduloJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]

  def collectResults(f: CompiledFilter[AnyTy]): CompiledFilter[AnyTy]

}
