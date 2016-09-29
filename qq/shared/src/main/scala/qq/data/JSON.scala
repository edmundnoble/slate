package qq.data

import qq.util.Recursion.RecursiveFunction
import qq.util.Unsafe
import upickle.Js

import scala.collection.generic.CanBuildFrom
import scalaz.Free.Trampoline
import scalaz.Trampoline

sealed trait JSON extends Any

object JSON {

  def upickleToJSONRec: RecursiveFunction[Js.Value, JSON] = new RecursiveFunction[Js.Value, JSON] {
    override def run(in: Js.Value, loop: (Js.Value) => Trampoline[JSON]): Trampoline[JSON] = in match {
      case Js.Str(s) => Trampoline.done(JSON.Str(s))
      case Js.Num(n) => Trampoline.done(JSON.Num(n))
      case Js.True => Trampoline.done(JSON.True)
      case Js.False => Trampoline.done(JSON.False)
      case Js.Null => Trampoline.done(JSON.Null)
      case Js.Arr(children@_*) =>
        Unsafe.builderTraverse[Seq]
          .traverse[Trampoline, Js.Value, JSON](children)(loop)
          .map(JSON.Arr(_: _*))
      case obj: Js.Obj =>
        Unsafe.builderTraverse[Seq]
          .traverse[Trampoline, (String, Js.Value), (String, JSON)](obj.value) { case (s, d) => loop(d) map (r => (s, r)) }
          .map(JSON.ObjList(_: _*))
    }
  }

  case object True extends JSON
  case object False extends JSON
  case object Null extends JSON
  final case class Str(value: String) extends AnyVal with JSON
  final case class Num(value: Double) extends AnyVal with JSON
  sealed abstract class Obj extends JSON {
    def toMap: ObjMap
    def toList: ObjList
    def map[B, That](f: ((String, JSON)) => B)(implicit cbf: CanBuildFrom[Any, B, That]): That
  }
  final case class ObjList(value: List[(String, JSON)]) extends Obj {
    override def toMap: ObjMap = ObjMap(value.toMap)
    override def toList: ObjList = this
    override def map[B, That](f: ((String, JSON)) => B)(implicit cbf: CanBuildFrom[Any, B, That]): That =
      value.map(f)(cbf)
  }
  object ObjList {
    def apply(values: (String, JSON)*): ObjList = ObjList(values.toList)
    def apply(): ObjList = ObjList(Nil)
  }
  final case class ObjMap(value: Map[String, JSON]) extends Obj {
    override def toMap: ObjMap = this
    override def toList: ObjList = ObjList(value.toList)
    override def map[B, That](f: ((String, JSON)) => B)(implicit cbf: CanBuildFrom[Any, B, That]): That =
      value.map(f)(cbf)
  }
  final case class Arr(value: List[JSON]) extends JSON
  object Arr {
    def apply(values: JSON*): Arr = Arr(values.toList)
  }
}
