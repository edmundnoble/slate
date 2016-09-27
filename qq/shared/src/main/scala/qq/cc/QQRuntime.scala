package qq
package cc

import monix.eval.Task
import monix.scalaz._
import monocle.{PTraversal, Setter, Traversal}
import qq.data._

import scala.language.higherKinds
import scalaz.{Applicative, \/}
import scalaz.syntax.plusEmpty._
import scalaz.std.list._
import scalaz.syntax.foldable1._
import scalaz.syntax.traverse._
import scalaz.syntax.std.option._

trait QQRuntime[J] {

  def print(value: J): String

  def platformPrelude: PlatformPrelude[J]

  def enjectFilter(obj: List[(\/[String, CompiledFilter[J]], CompiledFilter[J])]): CompiledFilter[J]

  @inline final def evaluateLeaf(component: LeafComponent[J]): CompiledFilter[J] = component match {
    case Dereference(name) =>
      (bindings: VarBindings[J]) =>
        bindings.get(name).cata(
          p => (_: J) => Task.now(p.value :: Nil),
          (_: J) => Task.raiseError(QQRuntimeException(s"Variable $name not bound"))
        )
    case ConstNumber(num) => constNumber(num)
    case ConstString(str) => constString(str)
  }

  @inline final def makePathComponentGetter(component: PathComponent): CompiledProgram[J] = component match {
    case CollectResults => collectResults
    case SelectKey(key) => selectKey(key)
    case SelectIndex(index) => selectIndex(index)
    case SelectRange(start, end) => selectRange(start, end)
  }

  def setPath(components: List[PathComponent], biggerStructure: J, smallerStructure: J): Task[List[J]]

  def modifyPath(component: PathComponent)(f: CompiledProgram[J]): CompiledProgram[J]

  @inline final def evaluatePath(components: List[PathComponent], operation: PathOperationF[CompiledProgram[J]]): CompiledProgram[J] = operation match {
    case PathGet() =>
      components.map(makePathComponentGetter)
        .nelFoldLeft1(CompiledProgram.id[J])(CompiledProgram.composePrograms[J])
    case PathSet(set) => (j: J) =>
      val app = set(j)
      app.flatMap {
        _.traverse[Task, List[J]] { jj =>
          setPath(components, j, jj)
        }
      }.map(_.flatten)
    case PathModify(modify) =>
      components.map(modifyPath).reduce((f, s) => (i: CompiledProgram[J]) => f(s(i)))(modify)
  }

  def selectKey(key: String): CompiledProgram[J]

  def selectIndex(index: Int): CompiledProgram[J]

  def selectRange(start: Int, end: Int): CompiledProgram[J]

  def constNumber(num: Double): CompiledFilter[J]

  def constString(str: String): CompiledFilter[J]

  def addJsValues(first: J, second: J): Task[J]

  def subtractJsValues(first: J, second: J): Task[J]

  def multiplyJsValues(first: J, second: J): Task[J]

  def divideJsValues(first: J, second: J): Task[J]

  def moduloJsValues(first: J, second: J): Task[J]

  def collectResults: CompiledProgram[J]

  def enlistFilter(filter: CompiledFilter[J]): CompiledFilter[J]

}
