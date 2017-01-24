package qq
package cc

import qq.data.ast._

trait QQRuntime[C] {

  def evaluatePath(components: Vector[PathComponent], operation: PathOperationF[C]): C

  def dereference(name: String): C

  def filterNot(): C

  def filterMath(first: C, second: C, op: MathOperator): C

  def silenceExceptions(f: C): C

  def constNumber(num: Double): C

  def constString(str: String): C

  def constBoolean(bool: Boolean): C

  def enlistFilter(filter: C): C

  def enjectFilter(obj: Vector[(String Either C, C)]): C

  def asBinding(name: String, as: C, in: C): C

  def ensequence(first: C, second: C): C

  def composeFilters(f: C, s: C): C

}
