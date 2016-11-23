package qq
package data

import cats.Eval
import cats.implicits._
import qq.cc.VectorToNelOps
import qq.util.Recursion.RecursiveFunction
import qq.util.Unsafe
import upickle.Js

import scala.collection.generic.CanBuildFrom

sealed trait JSON {
  override def toString: String = JSON.render(this)
}

object JSON {

  def upickleToJSONRec: RecursiveFunction[Js.Value, JSON] = new RecursiveFunction[Js.Value, JSON] {
    override def run(in: Js.Value, loop: (Js.Value) => Eval[JSON]): Eval[JSON] = in match {
      case Js.Str(s) => Eval.now(JSON.Str(s))
      case Js.Num(n) => Eval.now(JSON.Num(n))
      case Js.True => Eval.now(JSON.True)
      case Js.False => Eval.now(JSON.False)
      case Js.Null => Eval.now(JSON.Null)
      case Js.Arr(children@_*) =>
        Unsafe.builderTraverse[Seq]
          .traverse[Eval, Js.Value, JSON](children)(loop)
          .map(JSON.Arr(_: _*))
      case obj: Js.Obj =>
        Unsafe.builderTraverse[Seq]
          .traverse[Eval, (String, Js.Value), (String, JSON)](obj.value) { case (s, d) => loop(d) map (r => (s, r)) }
          .map(s => JSON.ObjList(s.toVector))
    }
  }

  def JSONToUpickleRec: RecursiveFunction[JSON, Js.Value] = new RecursiveFunction[JSON, Js.Value] {
    override def run(in: JSON, loop: (JSON) => Eval[Js.Value]): Eval[Js.Value] = in match {
      case JSON.Str(s) => Eval.now(Js.Str(s))
      case JSON.Num(n) => Eval.now(Js.Num(n))
      case JSON.True => Eval.now(Js.True)
      case JSON.False => Eval.now(Js.False)
      case JSON.Null => Eval.now(Js.Null)
      case JSON.Arr(children) =>
        children.traverse(loop)
          .map(Js.Arr(_: _*))
      case obj: JSON.Obj =>
        obj.toList.value
          .traverse[Eval, (String, Js.Value)] { case (s, d) => loop(d) map (r => (s, r)) }
          .map(Js.Obj(_: _*))
    }
  }

  def renderBare(v: JSON): String = v match {
    case JSON.Str(s) => s
    case _ => render(v)
  }

  def render(v: JSON): String = {
    def renderRec(v: JSON): Vector[String] = v match {
      case JSON.Str(s) => Vector("\"", s, "\"")
      case JSON.Num(n) =>
        val toInt = n.toInt
        (
          if (toInt == n) String.format("%d", Int.box(toInt))
          else String.format("%f", Double.box(n))
          ) +: Vector.empty[String]
      case JSON.True => "true" +: Vector.empty[String]
      case JSON.False => "false" +: Vector.empty[String]
      case JSON.Null => "null" +: Vector.empty[String]
      case JSON.Arr(vs) => "[" +: vs.map(renderRec).nelFoldLeft1(Vector.empty[String])((a, b) => (a :+ ",\n") ++ b) :+ "]"
      case o: JSON.Obj => "{" +: o.toList.value.map { case (k, nv) => Vector(k, ": ") ++ renderRec(nv) }.nelFoldLeft1(Vector.empty[String])((a, b) => (a :+ ",\n") ++ b) :+ "}"
    }

    renderRec(v).mkString
  }

  case object True extends JSON
  def `true`: JSON = True

  case object False extends JSON
  def `false`: JSON = False

  case object Null extends JSON
  def `null`: JSON = Null

  final case class Str(value: String) extends JSON
  def str(value: String): JSON = Str(value)

  final case class Num(value: Double) extends JSON
  def num(value: Double): JSON = Num(value)

  sealed trait Obj extends JSON {
    def toMap: ObjMap

    def toList: ObjList

    def map[B, That](f: ((String, JSON)) => B)(implicit cbf: CanBuildFrom[Any, B, That]): That
  }
  object Obj {
    def apply(values: (String, JSON)*): ObjList = ObjList(values.toVector)
    private[Obj] val empty = ObjList(Vector.empty)
    def apply(): ObjList = empty
  }
  final case class ObjList(value: Vector[(String, JSON)]) extends Obj {
    override def toMap: ObjMap = ObjMap(value.toMap)
    override def toList: ObjList = this
    override def map[B, That](f: ((String, JSON)) => B)(implicit cbf: CanBuildFrom[Any, B, That]): That =
      value.map(f)(cbf)
  }
  final case class ObjMap(value: Map[String, JSON]) extends Obj {
    override def toMap: ObjMap = this
    override def toList: ObjList = ObjList(value.toVector)
    override def map[B, That](f: ((String, JSON)) => B)(implicit cbf: CanBuildFrom[Any, B, That]): That =
      value.map(f)(cbf)
  }
  def obj(values: (String, JSON)*): JSON = Obj(values: _*)

  final case class Arr(value: Vector[JSON]) extends JSON
  object Arr {
    def apply(values: JSON*): Arr = Arr(values.toVector)
  }
  def arr(values: JSON*): JSON = Arr(values.toVector)
}
