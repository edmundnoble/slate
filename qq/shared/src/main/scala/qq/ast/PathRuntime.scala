package qq.ast

import cats.{Apply, Cartesian}
import cats.implicits._
import cats.functor.Invariant

trait PathTypeRuntime[C] {

  type P

  val collectResults: P

  def selectKey(key: String): P

  def selectIndex(index: Int): P

  def selectRange(start: Int, end: Int): P

  final def renderSingleInitial(comp: PathComponent): P = comp match {
    case CollectResults => collectResults
    case SelectKey(key) => selectKey(key)
    case SelectIndex(idx) => selectIndex(idx)
    case SelectRange(start, end) => selectRange(start, end)
  }

  final def renderInitial(initial: Vector[PathComponent]): P =
    if (initial.isEmpty) empty
    else initial.foldLeft(empty)((p, x) => append(p, renderSingleInitial(x)))

  def empty: P

  def append(p1: P, p2: P): P

  def ret(p: P): C

}

object PathTypeRuntime {
  type Aux[C, P0] = PathTypeRuntime[C] {type P = P0}
  implicit val invariantCartesian: Apply[PathTypeRuntime] = new Apply[PathTypeRuntime] {
    override def ap[A, B](ff: PathTypeRuntime[(A) => B])(fa: PathTypeRuntime[A]): PathTypeRuntime[B] = new PathTypeRuntime[B] {
      override type P = (ff.P, fa.P)
      override val collectResults: (ff.P, fa.P) = (ff.collectResults, fa.collectResults)

      override def selectKey(key: String): (ff.P, fa.P) = (ff.selectKey(key), fa.selectKey(key))

      override def selectIndex(index: Int): (ff.P, fa.P) = (ff.selectIndex(index), fa.selectIndex(index))

      override def selectRange(start: Int, end: Int): (ff.P, fa.P) = (ff.selectRange(start, end), fa.selectRange(start, end))

      override def empty: (ff.P, fa.P) = (ff.empty, fa.empty)

      override def append(p1: (ff.P, fa.P), p2: (ff.P, fa.P)): (ff.P, fa.P) = (ff.append(p1._1, p2._1), fa.append(p1._2, p2._2))

      override def ret(p: (ff.P, fa.P)): B = ff.ret(p._1)(fa.ret(p._2))
    }

    override def map[A, B](fa: PathTypeRuntime[A])(f: (A) => B): PathTypeRuntime[B] = new PathTypeRuntime[B] {
      override type P = fa.P
      override val collectResults: fa.P = fa.collectResults

      override def selectKey(key: String): fa.P = fa.selectKey(key)

      override def selectIndex(index: Int): fa.P = fa.selectIndex(index)

      override def selectRange(start: Int, end: Int): fa.P = fa.selectRange(start, end)

      override def empty: fa.P = fa.empty

      override def append(p1: fa.P, p2: fa.P): fa.P = fa.append(p1, p2)

      override def ret(p: fa.P): B = f(fa.ret(p))
    }
  }
}

trait PathRuntime[C] {
  def set(c: C): PathTypeRuntime[C]

  def modify(c: C): PathTypeRuntime[C]

  val get: PathTypeRuntime[C]
}

object PathRuntime {
  implicit val invariantCartesian: Invariant[PathRuntime] with Cartesian[PathRuntime] = new Invariant[PathRuntime] with Cartesian[PathRuntime] {
    override def imap[A, B](fa: PathRuntime[A])(f: (A) => B)(g: (B) => A): PathRuntime[B] = new PathRuntime[B] {
      override def set(c: B): PathTypeRuntime[B] = fa.set(g(c)).map(f)

      override def modify(c: B): PathTypeRuntime[B] = fa.modify(g(c)).map(f)

      override val get: PathTypeRuntime[B] = fa.get.map(f)
    }

    override def product[A, B](fa: PathRuntime[A], fb: PathRuntime[B]): PathRuntime[(A, B)] = new PathRuntime[(A, B)] {
      override def set(c: (A, B)): PathTypeRuntime[(A, B)] = fa.set(c._1).product(fb.set(c._2))

      override def modify(c: (A, B)): PathTypeRuntime[(A, B)] = fa.modify(c._1).product(fb.modify(c._2))

      override val get: PathTypeRuntime[(A, B)] = fa.get.product(fb.get)
    }
  }
}
