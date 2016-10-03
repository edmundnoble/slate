package qq.data

import java.text.DecimalFormat

import qq.util.Recursion.RecursiveFunction
import qq.util.Unsafe
import upickle.Js

import scala.collection.generic.CanBuildFrom
import scalaz.Free.Trampoline
import scalaz.Trampoline
import scalaz.std.list._
import scalaz.syntax.traverse._
import qq.cc.ListToNelOps

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
          .map(s => JSON.ObjList(s.toList))
    }
  }

  def JSONToUpickleRec: RecursiveFunction[JSON, Js.Value] = new RecursiveFunction[JSON, Js.Value] {
    override def run(in: JSON, loop: (JSON) => Trampoline[Js.Value]): Trampoline[Js.Value] = in match {
      case JSON.Str(s) => Trampoline.done(Js.Str(s))
      case JSON.Num(n) => Trampoline.done(Js.Num(n))
      case JSON.True => Trampoline.done(Js.True)
      case JSON.False => Trampoline.done(Js.False)
      case JSON.Null => Trampoline.done(Js.Null)
      case JSON.Arr(children) =>
        children.traverse(loop)
          .map(Js.Arr(_: _*))
      case obj: JSON.Obj =>
        obj.toList.value
          .traverse[Trampoline, (String, Js.Value)] { case (s, d) => loop(d) map (r => (s, r)) }
          .map(Js.Obj(_: _*))
    }
  }

  def render(v: JSON): String = {
    def renderRec(v: JSON): Vector[String] = v match {
      case JSON.Str(s) => Vector("\"", s, "\"")
      case JSON.Num(n) => {
        val toInt = n.toInt
        {
          if (toInt == n) String.format("%d", Int.box(toInt))
          else String.format("%f", Double.box(n))
        } +: Vector.empty[String]
      }
      case JSON.True => "true" +: Vector.empty[String]
      case JSON.False => "false" +: Vector.empty[String]
      case JSON.Null => "null" +: Vector.empty[String]
      case JSON.Arr(v) => "[" +: v.map(renderRec).nelFoldLeft1(Vector.empty[String])((a, b) => (a :+ ",\n") ++ b) :+ "]"
      case o: JSON.Obj => "{" +: o.toList.value.map { case (k, nv) => Vector(k, ": ") ++ renderRec(nv) }.nelFoldLeft1(Vector.empty[String])((a, b) => (a :+ ",\n") ++ b) :+ "}"
    }

    renderRec(v).mkString
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
  object Obj {
    def apply(values: (String, JSON)*): ObjList = ObjList(values.toList)
    private[Obj] val empty = ObjList(Nil)
    def apply(): ObjList = empty
  }
  final case class ObjList(value: List[(String, JSON)]) extends Obj {
    override def toMap: ObjMap = ObjMap(value.toMap)
    override def toList: ObjList = this
    override def map[B, That](f: ((String, JSON)) => B)(implicit cbf: CanBuildFrom[Any, B, That]): That =
      value.map(f)(cbf)
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
