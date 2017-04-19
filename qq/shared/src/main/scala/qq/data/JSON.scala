package qq
package data

import cats.data.Ior
import cats.{Eval, Functor}
import cats.free.Free
import cats.implicits._
import qq.cc.VectorToNelOps
import qq.util.Recursion.RecursiveFunction
import qq.util.Unsafe
import shapeless.tag.@@
import upickle.Js

import scala.collection.generic.CanBuildFrom

sealed trait JSON {
  override def toString: String = JSON.render(this)
}

sealed trait SJSON extends JSON
sealed trait LJSON extends JSON

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

  case object True extends LJSON
  def `true`: JSON = True

  case object False extends LJSON
  def `false`: JSON = False

  case object Null extends LJSON
  def `null`: JSON = Null

  final case class Str(value: String) extends LJSON
  def str(value: String): JSON = Str(value)

  final case class Num(value: Double) extends LJSON
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
  final case class ObjList(value: Vector[(String, JSON)]) extends Obj with SJSON {
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

  final case class Arr(value: Vector[JSON]) extends SJSON
  object Arr {
    def apply(values: JSON*): Arr = Arr(values.toVector)
  }
  def arr(values: JSON*): JSON = Arr(values.toVector)

  sealed trait JSONOrigin
  case object Top extends JSONOrigin
  case object Bottom extends JSONOrigin

  sealed trait JSONModification {
    def changesShape: Boolean
  }
  final case class AddTo(origin: JSONOrigin) extends JSONModification {
    def changesShape: Boolean = true
  }
  final case class DeleteFrom(origin: JSONOrigin) extends JSONModification {
    def changesShape: Boolean = true
  }
  final case class SetTo(newValue: LJSON) extends JSONModification {
    def changesShape: Boolean = false
  }
  final case class RecIdx(index: Int, modification: JSONModification) extends JSONModification {
    def changesShape: Boolean = modification.changesShape
  }
  final case class UpdateKey(index: Int, newKey: String) extends JSONModification {
    def changesShape: Boolean = false
  }

  // Tag indicating data originates from a potential modification to some other dataset
  sealed trait Modified

  import shapeless.tag

  val modifiedTag: tag.Tagger[Modified] = tag[Modified]

  // The branching functor for JSON, except each object key might be modified.
  sealed trait ModifiedJSONF[A]
  final case class ArrayF[A](values: Vector[A]) extends ModifiedJSONF[A]
  final case class ObjectF[A](entries: Vector[ObjectEntry[A]]) extends ModifiedJSONF[A]
  final case class ObjectEntry[A](key: Ior[String @@ Modified, String], value: A) {
    def map[B](f: A => B): ObjectEntry[B] = copy(value = f(value))
  }

  def arrayf[A](values: Vector[A]): ModifiedJSONF[A] = ArrayF(values)
  def objectf[A](entries: Vector[ObjectEntry[A]]): ModifiedJSONF[A] = ObjectF(entries)

  object ModifiedJSONF {
    implicit val modifiedJSONFFunctorInstance: Functor[ModifiedJSONF] = new Functor[ModifiedJSONF] {
      override def map[A, B](fa: ModifiedJSONF[A])(f: (A) => B): ModifiedJSONF[B] = fa match {
        case ArrayF(values) => ArrayF(values.map(f))
        case ObjectF(entries) => ObjectF(entries.map(_.map(f)))
      }
    }
  }

  // A json value with parts which might be modified!
  // Really a tree where the branches are ModifiedJSONF and the leaves are either JSON trees or modified JSON trees.
  type ModifiedJSON = Free[ModifiedJSONF, Ior[LJSON @@ Modified, LJSON]]

  def unmodified(json: JSON): ModifiedJSON = json match {
    case l: LJSON => Free.pure(Ior.right(l))
    case Arr(a) => Free.roll(arrayf(a.map(unmodified)))
    case ObjList(o) => Free.roll(objectf(o.map { case (k, v) => ObjectEntry[ModifiedJSON](Ior.right(k), unmodified(v)) }))
    case o: ObjMap => unmodified(o.toList)
  }
  def modified(json: JSON): ModifiedJSON = json match {
    case l: LJSON => Free.pure(Ior.left(modifiedTag(l)))
    case Arr(a) => Free.roll(arrayf(a.map(modified)))
    case ObjList(o) => Free.roll(objectf(o.map { case (k, v) => ObjectEntry[ModifiedJSON](Ior.left(modifiedTag(k)), modified(v)) }))
    case o: ObjMap => modified(o.toList)
  }

  def fromIsModified(json: LJSON, isModified: Boolean): ModifiedJSON =
    Free.pure(if (isModified) Ior.left(modifiedTag(json)) else Ior.right(json))

  def commit(modifiedJSON: ModifiedJSON): JSON = modifiedJSON.fold(_.fold[LJSON](identity, identity, (m, _) => m), {
    case ArrayF(fs) => Arr(fs.map(commit))
    case ObjectF(fs) => ObjList(fs.map { e => (e.key.fold[String](identity, identity, (m, _) => m), commit(e.value)) })
  })

  final implicit class modificationsOps(mods: List[JSONModification]) {
    def zoom(index: Int): List[JSONModification] = mods.foldLeft((index, mods)) {
      case ((i, ms), mod) =>
        mod match {
          case AddTo(_) => (i - 1, ms)
          case DeleteFrom(_) => (i + 1, ms)
          case RecIdx(ni, m) if ni == i => (ni, m :: ms)
          case _ => (i, ms)
        }
    }._2
    def apply(json: JSON, defaultArrElem: LJSON, defaultObjElem: (String, LJSON)): Option[ModifiedJSON] = mods.foldM[Option, ModifiedJSON](unmodified(json)) { (j, mod) =>
      mod(j, defaultArrElem, defaultObjElem)
    }
  }

  final implicit class iorOps[E, A](io: E Ior A) {
    def putLeft(e: E): E Ior A =
      io.right.fold[E Ior A](Ior.left(e))(Ior.both(e, _))
    def putRight(a: A): E Ior A =
      io.left.fold[E Ior A](Ior.right(a))(Ior.both(_, a))
  }

  final implicit class freeCompanionOps(comp: Free.type) {
    def roll[F[_], A](rolly: F[Free[F, A]]): Free[F, A] =
      Free.liftF(rolly).flatMap(identity)
  }

  def adjoin[J](seq: Vector[J], elem: J, origin: JSONOrigin): Vector[J] = if (origin == Top) elem +: seq else seq :+ elem
  def deleteFrom[J](seq: Vector[J], origin: JSONOrigin): Vector[J] = if (origin == Top) seq.tail else seq.init

  final implicit class modificationOps(mod: JSONModification) {
    def apply(json: ModifiedJSON, defaultArrElem: LJSON, defaultObjElem: (String, LJSON)): Option[ModifiedJSON] = {
      mod match {
        case AddTo(origin) =>
          json.resume match {
            case Left(ObjectF(oldObjectEntries)) =>
              val newObjectEntry =
                ObjectEntry[ModifiedJSON](
                  Ior.left(modifiedTag(defaultObjElem._1)),
                  Free.pure(Ior.left[LJSON @@ Modified, LJSON](modifiedTag(defaultObjElem._2)))
                )
              Some(Free.roll(objectf(adjoin(oldObjectEntries, newObjectEntry, origin))))
            case Left(ArrayF(oldArrayEntries)) =>
              val newArrayElem: ModifiedJSON =
                Free.pure(Ior.left[LJSON @@ Modified, LJSON](modifiedTag(defaultArrElem)))
              Some(Free.roll(arrayf(adjoin(oldArrayEntries, newArrayElem, origin))))
            case _ => None
          }
        case DeleteFrom(origin) => json.resume match {
          case Left(ObjectF(o)) if o.nonEmpty => Some(Free.roll(objectf(deleteFrom(o, origin))))
          case Left(ArrayF(a)) if a.nonEmpty => Some(Free.roll(arrayf(deleteFrom(a, origin))))
          case _ => None
        }
        case RecIdx(i, m) => json.resume match {
          case Left(ObjectF(o)) if o.length > i =>
            val elem = o(i)
            m(elem.value, defaultArrElem, defaultObjElem).map(nj =>
              Free.roll(objectf(o.updated(i, elem.copy(value = nj)))))
          case Left(ArrayF(a)) if a.length > i =>
            m(a(i), defaultArrElem, defaultObjElem).map(nj =>
              Free.roll(arrayf(a.updated(i, nj))))
          case _ => None
        }
        case UpdateKey(i, k) => json.resume match {
          case Left(ObjectF(o)) if o.length > i =>
            val elem = o(i)
            Some(Free.roll(objectf(
              o.updated(i, elem.copy(key = elem.key.putLeft(modifiedTag(k))))
            )))
          case _ => None
        }
        case SetTo(newValue) => json.resume match {
          case Right(modifiedValues) =>
            Some(Free.pure(modifiedValues.putLeft(modifiedTag(newValue))))
          case _ => None
        }
      }
    }
  }

  import fastparse.all._

  def decompose(json: JSON): LJSON Either SJSON = json match {
    case l: LJSON => Left(l)
    case m: ObjMap => Right(m.toList)
    case s: SJSON => Right(s)
  }

  val Digits = '0' to '9' contains (_: Char)
  val digits = P(CharsWhile(Digits))
  val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
  val fractional = P("." ~ digits)
  val integral = P("0" | CharIn('1' to '9') ~ digits.?)
  val number = P((CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(_.toDouble))
  val parserForLJSON: P[LJSON] = P(
    qq.cc.Parser.escapedStringLiteral.map(Str) |
      number.map(Num) |
      LiteralStr("false").map(_ => False) |
      LiteralStr("true").map(_ => True) |
      LiteralStr("null").map(_ => Null)
  )

  def renderLJSON(l: LJSON): String = l match {
    case Str(string) => "\"" + string + "\""
    case Num(num) => num.toString
    case True => "true"
    case False => "false"
    case Null => "null"
  }

}
