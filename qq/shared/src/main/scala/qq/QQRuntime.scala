package qq

import monix.eval.Task

import scalaz.\/

trait QQRuntime[AnyTy] {

  import QQCompiler._

  def platformPrelude: PlatformPrelude[AnyTy]

  def enjectFilter(obj: List[(\/[String, CompiledFilter[AnyTy]], CompiledFilter[AnyTy])]): CompiledFilter[AnyTy]

  def enlistFilter(filter: CompiledFilter[AnyTy]): CompiledFilter[AnyTy]

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
