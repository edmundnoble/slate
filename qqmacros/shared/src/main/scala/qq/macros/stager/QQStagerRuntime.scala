package qq.macros.stager

import qq.cc.QQRuntime
import qq.data.ast.{MathOperator, PathComponent, PathOperationF}

import scala.reflect.macros.whitebox

class QQStagerRuntime(c: whitebox.Context) extends QQRuntime[c.Tree] {
  import c.universe._

  override def evaluatePath(components: Vector[PathComponent], operation: PathOperationF[c.Tree]): c.Tree =
    q"qq.cc.QQInterpreterRuntime.evaluatePath($components, $operation)"

  override def dereference(name: String): c.Tree =
    q"qq.cc.QQInterpreterRuntime.dereference(name)"

  override def filterNot(): c.Tree =
    q"qq.cc.QQInterpreterRuntime.filterNot()"

  override def filterMath(first: c.Tree, second: c.Tree, op: MathOperator): c.Tree =
    q"qq.cc.QQInterpreterRuntime.filterMath($first, $second, $op)"

  override def silenceExceptions(f: c.Tree): c.Tree =
    q"qq.cc.QQInterpreterRuntime.silenceExceptions($f)"

  override def constNumber(num: Double): c.Tree =
    q"qq.cc.QQInterpreterRuntime.constNumber($num)"

  override def constString(str: String): c.Tree =
    q"qq.cc.QQInterpreterRuntime.constString($str)"

  override def constBoolean(bool: Boolean): c.Tree =
    q"qq.cc.QQInterpreterRuntime.constBoolean($bool)"

  override def enlistFilter(filter: c.Tree): c.Tree =
    q"qq.cc.QQInterpreterRuntime.enlistFilter($filter)"

  override def enjectFilter(obj: Vector[(Either[String, c.Tree], c.Tree)]): c.Tree =
    q"qq.cc.QQInterpreterRuntime.enjectFilter($obj)"

  override def asBinding(name: String, as: c.Tree, in: c.Tree): c.Tree =
    q"qq.cc.QQInterpreterRuntime.asBinding($name, $as, $in)"

  override def ensequence(first: c.Tree, second: c.Tree): c.Tree =
    q"qq.cc.QQInterpreterRuntime.ensequence($first, $second)"

  override def composeFilters(f: c.Tree, s: c.Tree): c.Tree =
    q"qq.cc.QQInterpreterRuntime.composeFilters($f, $s)"

}
