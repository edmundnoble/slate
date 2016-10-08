package qq
package cc

import monix.eval.{Coeval, Task}
import monix.scalaz._
import qq.data._

import scala.language.higherKinds
import scalaz.\/

trait QQRuntime[J] {

  def print(value: J): String

  def platformPrelude: PlatformPrelude[J]

  def enjectFilter(obj: List[(String \/ CompiledFilter[J], CompiledFilter[J])]): CompiledFilter[J]

  def setPath(components: List[PathComponent], biggerStructure: J, smallerStructure: J): Task[List[J]]

  def modifyPath(component: PathComponent)(f: CompiledProgram[J]): CompiledProgram[J]

  def selectKey(key: String): CompiledProgram[J]

  def selectIndex(index: Int): CompiledProgram[J]

  def selectRange(start: Int, end: Int): CompiledProgram[J]

  def constNumber(num: Double): CompiledFilter[J]

  def constString(str: String): CompiledFilter[J]

  def constBoolean(bool: Boolean): CompiledFilter[J]

  def addJsValues(first: J, second: J): Task[J]

  def subtractJsValues(first: J, second: J): Task[J]

  def multiplyJsValues(first: J, second: J): Task[J]

  def divideJsValues(first: J, second: J): Task[J]

  def moduloJsValues(first: J, second: J): Task[J]

  def equalJsValues(first: J, second: J): J

  def lteJsValues(j1: J, j2: J): J = ???
  def gteJsValues(j1: J, j2: J): J = ???
  def lessThanJsValues(j1: J, j2: J): J = ???
  def greaterThanJsValues(j1: J, j2: J): J = ???

  def not(v: J): Coeval[J]

  def collectResults: CompiledProgram[J]

  def enlistFilter(filter: CompiledFilter[J]): CompiledFilter[J]

}
