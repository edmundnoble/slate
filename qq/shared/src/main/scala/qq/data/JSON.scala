package qq
package data

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
    def changesShape: Eval[Boolean]
  }
  final case class AddTo(origin: JSONOrigin) extends JSONModification {
    override def changesShape: Eval[Boolean] = Eval.now(true)
  }
  final case class DeleteFrom(origin: JSONOrigin) extends JSONModification {
    override def changesShape: Eval[Boolean] = Eval.now(true)
  }
  final case class SetTo(newValue: JSON) extends JSONModification {
    override def changesShape: Eval[Boolean] = Eval.now(false)
  }
  final case class RecIdx(index: Int, modification: JSONModification) extends JSONModification {
    override def changesShape: Eval[Boolean] = Eval.defer(modification.changesShape)
  }
  final case class UpdateKey(index: Int, newKey: String) extends JSONModification {
    override def changesShape: Eval[Boolean] = Eval.now(false)
  }

  // Tag indicating data originates from a potential modification to some other dataset
  sealed trait Modified

  import shapeless.tag

  val modifiedTag = tag[Modified]

  // The branching functor for JSON, except each object key might be modified.
  sealed trait ModifiedJSONF[A]
  final case class ArrayF[A](values: Vector[A]) extends ModifiedJSONF[A]
  final case class ObjectF[A](entries: Vector[ObjectEntry[A]]) extends ModifiedJSONF[A]
  final case class ObjectEntry[A](modified: Boolean, key: String, value: A) {
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
  type ModifiedJSON = Free[ModifiedJSONF, (JSON @@ Modified) Either JSON]

  def unmodified(json: JSON): ModifiedJSON = Free.pure(Right(json))
  def modified(json: JSON): ModifiedJSON = Free.pure(Left(modifiedTag(json)))

  def fromIsModified(json: JSON, isModified: Boolean): ModifiedJSON =
    Free.pure(if (isModified) Left(modifiedTag(json)) else Right(json))

  def commit(modifiedJSON: ModifiedJSON): JSON = modifiedJSON.fold(_.merge, {
    case ArrayF(fs) => Arr(fs.map(commit))
    case ObjectF(fs) => ObjList(fs.map { e => (e.key, commit(e.value)) })
  })

  final implicit class modificationsOps(mods: List[JSONModification]) {
    def zoom(index: Int): List[JSONModification] = mods.foldLeft[(Int, List[JSONModification])]((index, mods)) {
      case ((i, ms), mod) =>
        mod match {
          case AddTo(_) => (i - 1, ms)
          case DeleteFrom(_) => (i + 1, ms)
          case RecIdx(ni, m) if ni == i => (ni, m :: ms)
          case _ => (i, ms)
        }
    }._2
    def apply(json: JSON, defaultArrElem: JSON, defaultObjElem: (String, JSON)): Option[ModifiedJSON] = mods.foldM[Option, ModifiedJSON](unmodified(json)) { (j, mod) =>
      mod(j, defaultArrElem, defaultObjElem)
    }
  }

  final implicit class modificationOps(mod: JSONModification) {
    def apply(json: ModifiedJSON, defaultArrElem: JSON, defaultObjElem: (String, JSON)): Option[ModifiedJSON] = {
      val newArrayElem: Free[ModifiedJSONF, Either[JSON @@ Modified, JSON]] = Free.pure(Left(modifiedTag(defaultArrElem)))
      val newObjectEntry: ObjectEntry[ModifiedJSON] = ObjectEntry[ModifiedJSON](modified = true, defaultObjElem._1, Free.pure(Left(modifiedTag(defaultObjElem._2))))
      def adjoin[J](seq: Vector[J], elem: J, origin: JSONOrigin): Vector[J] = if (origin == Top) elem +: seq else seq :+ elem
      def deleteFrom[J](seq: Vector[J], origin: JSONOrigin): Vector[J] = if (origin == Top) seq.tail else seq.init
      mod match {
        case AddTo(origin) => json.resume match {
          case Left(ObjectF(oldObjectEntries)) =>
            Some(Free.liftF(objectf(adjoin(oldObjectEntries, newObjectEntry, origin))).flatMap(identity))
          case Left(ArrayF(oldArrayEntries)) =>
            Some(Free.liftF(arrayf(adjoin(oldArrayEntries, newArrayElem, origin))).flatMap(identity))
          case Right(Right(j)) => j match {
            case ObjList(o) =>
              Some(Free.liftF(objectf(adjoin(o.map { case (k, v) => ObjectEntry(modified = false, k, unmodified(v)) }, newObjectEntry, origin))).flatMap(identity))
            case Arr(a) =>
              Some(Free.liftF(arrayf(adjoin(a.map(unmodified), newArrayElem, origin))).flatMap(identity))
            case _ => None
          }
          case Right(Left(mj)) => (mj: JSON) match {
            case ObjList(o) =>
              Some(Free.liftF(objectf(adjoin(o.map { case (k, v) => ObjectEntry(modified = true, k, modified(v)) }, newObjectEntry, origin))).flatMap(identity))
            case Arr(a) =>
              Some(Free.liftF(arrayf(adjoin(a.map(modified), newArrayElem, origin))).flatMap(identity))
            case _ => None
          }
          case _ => None
        }
        case DeleteFrom(origin) => json.resume match {
          case Left(ObjectF(o)) if o.nonEmpty => Some(Free.liftF(objectf(deleteFrom(o, origin))).flatMap(identity))
          case Left(ArrayF(a)) if a.nonEmpty => Some(Free.liftF(arrayf(deleteFrom(a, origin))).flatMap(identity))
          case Right(Right(j)) => j match {
            case ObjList(o) =>
              Some(Free.liftF(objectf(deleteFrom(o.map { case (k, v) => ObjectEntry(modified = false, k, unmodified(v)) }, origin))).flatMap(identity))
            case Arr(a) =>
              Some(Free.liftF(arrayf(deleteFrom(a.map(unmodified), origin))).flatMap(identity))
            case _ => None
          }
          case Right(Left(mj)) => (mj: JSON) match {
            case ObjList(o) =>
              Some(Free.liftF(objectf(deleteFrom(o.map { case (k, v) => ObjectEntry(modified = true, k, modified(v)) }, origin))).flatMap(identity))
            case Arr(a) =>
              Some(Free.liftF(arrayf(deleteFrom(a.map(modified), origin))).flatMap(identity))
            case _ => None
          }
          case _ => None
        }
        case RecIdx(i, m) => json.resume match {
          case Left(ObjectF(o)) if o.length > i =>
            val elem = o(i)
            m(elem.value, defaultArrElem, defaultObjElem).map(nj => Free.liftF(objectf(o.updated(i, elem.copy(value = nj)))).flatMap(identity))
          case Left(ArrayF(a)) if a.length > i =>
            m(a(i), defaultArrElem, defaultObjElem).map(nj => Free.liftF(arrayf(a.updated(i, nj))).flatMap(identity))
          case Right(Right(j)) => j match {
            case ObjList(o) =>
              val elem = o(i)
              m(unmodified(elem._2), defaultArrElem, defaultObjElem).map(nj =>
                Free.liftF(objectf(o.map { case (k, v) => ObjectEntry(modified = false, k, unmodified(v)) }.updated(i, ObjectEntry(modified = false, elem._1, nj)))).flatMap(identity)
              )
            case Arr(a) =>
              val elem = a(i)
              m(unmodified(elem), defaultArrElem, defaultObjElem).map(nj =>
                Free.liftF(arrayf[ModifiedJSON](a.map(unmodified).updated(i, nj))).flatMap(identity)
              )
            case _ => None
          }
          case Right(Left(mj)) => (mj: JSON) match {
            case ObjList(o) =>
              val elem = o(i)
              m(modified(elem._2), defaultArrElem, defaultObjElem).map(nj =>
                Free.liftF(objectf(o.map { case (k, v) => ObjectEntry(modified = false, k, modified(v)) }.updated(i, ObjectEntry(modified = false, elem._1, nj)))).flatMap(identity)
              )
            case Arr(a) =>
              val elem = a(i)
              m(modified(elem), defaultArrElem, defaultObjElem).map(nj =>
                Free.liftF(arrayf[ModifiedJSON](a.map(modified).updated(i, nj))).flatMap(identity)
              )
            case _ => None
          }
          case _ => None
        }
        case UpdateKey(i, k) => json.resume match {
          case Left(ObjectF(o)) if o.length > i =>
            Some(Free.liftF(objectf(o.updated(i, o(i).copy(modified = true, key = k)))).flatMap(identity))
          case Right(Right(ObjList(o))) if o.length > i =>
            Some(
              Free.liftF(
                objectf(
                  o.map { case (kn, v) => ObjectEntry(modified = false, kn, unmodified(v)) }
                    .updated(i, ObjectEntry(modified = true, k, unmodified(o(i)._2)))
                )
              ).flatMap(identity)
            )
          case Right(Left(nj)) => (nj: JSON) match {
            case ObjList(o) =>
              Some(
                Free.liftF(
                  objectf(
                    o.map { case (kn, v) => ObjectEntry(modified = false, kn, modified(v)) }
                      .updated(i, ObjectEntry(modified = true, k, modified(o(i)._2)))
                  )
                ).flatMap(identity)
              )
            case _ => None
          }
          case _ => None
        }
        case SetTo(newValue) => Some(Free.pure(Left(modifiedTag(newValue))))
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
