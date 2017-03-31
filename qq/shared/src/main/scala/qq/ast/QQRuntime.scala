package qq
package ast

import cats.functor.Invariant
import cats.{Cartesian, Functor}
import cats.implicits._

trait QQRuntime[C] {

  type Enject[A] = Vector[(String Either A, A)]

  implicit final val enjectFunctor = new Functor[Enject] {
    override def map[A, B](fa: Enject[A])(f: (A) => B): Enject[B] = fa.map { case (t, a) => (t.map(f), f(a)) }
  }

  val path: PathRuntime[C]

  def dereference(name: String): C

  val filterNot: C

  def add(first: C, second: C): C

  def subtract(first: C, second: C): C

  def multiply(first: C, second: C): C

  def divide(first: C, second: C): C

  def modulo(first: C, second: C): C

  def equal(first: C, second: C): C

  def lte(first: C, second: C): C

  def gte(first: C, second: C): C

  def lessThan(first: C, second: C): C

  def greaterThan(first: C, second: C): C

  def silenceExceptions(f: C): C

  def constNumber(num: Double): C

  def constString(str: String): C

  def constBoolean(bool: Boolean): C

  def enlistFilter(filter: C): C

  def enject(obj: Vector[(String Either C, C)]): C

  def asBinding(name: String, as: C, in: C): C

  def ensequence(first: C, second: C): C

  def composeFilters(f: C, s: C): C

}

object QQRuntime {
  implicit val invariantCartesian: Invariant[QQRuntime] with Cartesian[QQRuntime] = new Invariant[QQRuntime] with Cartesian[QQRuntime] {
    def product[A, B](fa: QQRuntime[A], fb: QQRuntime[B]): QQRuntime[(A, B)] = new QQRuntime[(A, B)] {
      def constBoolean(bool: Boolean): (A, B) = (fa.constBoolean(bool), fb.constBoolean(bool))

      def dereference(name: String): (A, B) = (fa.dereference(name), fb.dereference(name))

      def enject(obj: Vector[(Either[String, (A, B)], (A, B))]): (A, B) = (fa.enject(enjectFunctor.map(obj)(_._1)), fb.enject(enjectFunctor.map(obj)(_._2)))

      def enlistFilter(filter: (A, B)): (A, B) = (fa.enlistFilter(filter._1), fb.enlistFilter(filter._2))

      def divide(first: (A, B), second: (A, B)): (A, B) = (fa.divide(first._1, second._1), fb.divide(first._2, second._2))

      def multiply(first: (A, B), second: (A, B)): (A, B) = (fa.multiply(first._1, second._1), fb.multiply(first._2, second._2))

      def add(first: (A, B), second: (A, B)): (A, B) = (fa.add(first._1, second._1), fb.add(first._2, second._2))

      def constNumber(num: Double): (A, B) = (fa.constNumber(num: Double), fb.constNumber(num: Double))

      def subtract(first: (A, B), second: (A, B)): (A, B) = (fa.subtract(first._1, second._1), fb.subtract(first._2, second._2))

      def asBinding(name: String, as: (A, B), in: (A, B)): (A, B) = (fa.asBinding(name: String, as._1, in._1), fb.asBinding(name: String, as._2, in._2))

      def silenceExceptions(f: (A, B)): (A, B) = (fa.silenceExceptions(f._1), fb.silenceExceptions(f._2))

      def equal(first: (A, B), second: (A, B)): (A, B) = (fa.equal(first._1, second._1), fb.equal(first._2, second._2))

      def lte(first: (A, B), second: (A, B)): (A, B) = (fa.lte(first._1, second._1), fb.lte(first._2, second._2))

      def gte(first: (A, B), second: (A, B)): (A, B) = (fa.gte(first._1, second._1), fb.gte(first._2, second._2))

      def lessThan(first: (A, B), second: (A, B)): (A, B) = (fa.lessThan(first._1, second._1), fb.lessThan(first._2, second._2))

      def greaterThan(first: (A, B), second: (A, B)): (A, B) = (fa.greaterThan(first._1, second._1), fb.greaterThan(first._2, second._2))

      def constString(str: String): (A, B) = (fa.constString(str), fb.constString(str))

      def composeFilters(f: (A, B), s: (A, B)): (A, B) = (fa.composeFilters(f._1, s._1), fb.composeFilters(f._2, s._2))

      def ensequence(first: (A, B), second: (A, B)): (A, B) = (fa.ensequence(first._1, second._1), fb.ensequence(first._2, second._2))

      def modulo(first: (A, B), second: (A, B)): (A, B) = (fa.modulo(first._1, second._1), fb.modulo(first._2, second._2))

      val filterNot: (A, B) = (fa.filterNot, fb.filterNot)

      val path: PathRuntime[(A, B)] = fa.path.product(fb.path)
    }

    def imap[A, B](fa: QQRuntime[A])(f: (A) => B)(g: (B) => A): QQRuntime[B] = new QQRuntime[B] {
      def constBoolean(bool: Boolean): B = f(fa.constBoolean(bool))

      def dereference(name: String): B = f(fa.dereference(name))

      def enject(obj: Vector[(Either[String, B], B)]): B = f(fa.enject(obj.map { case (t, b) => (t.map(g), g(b)) }))

      def enlistFilter(filter: B): B = f(fa.enlistFilter(g(filter)))

      def divide(first: B, second: B): B = f(fa.divide(g(first), g(second)))

      def multiply(first: B, second: B): B = f(fa.multiply(g(first), g(second)))

      def add(first: B, second: B): B = f(fa.add(g(first), g(second)))

      def constNumber(num: Double): B = f(fa.constNumber(num))

      def subtract(first: B, second: B): B = f(fa.subtract(g(first), g(second)))

      def asBinding(name: String, as: B, in: B): B = f(fa.asBinding(name, g(as), g(in)))

      def silenceExceptions(filter: B): B = f(fa.silenceExceptions(g(filter)))

      def equal(first: B, second: B): B = f(fa.equal(g(first), g(second)))

      def lte(first: B, second: B): B = f(fa.lte(g(first), g(second)))

      def gte(first: B, second: B): B = f(fa.gte(g(first), g(second)))

      def lessThan(first: B, second: B): B = f(fa.lessThan(g(first), g(second)))

      def greaterThan(first: B, second: B): B = f(fa.greaterThan(g(first), g(second)))

      def constString(str: String): B = f(fa.constString(str))

      def composeFilters(first: B, second: B): B = f(fa.composeFilters(g(first), g(second)))

      def ensequence(first: B, second: B): B = f(fa.ensequence(g(first), g(second)))

      def modulo(first: B, second: B): B = f(fa.modulo(g(first), g(second)))

      val filterNot: B = f(fa.filterNot)

      val path: PathRuntime[B] = fa.path.imap(f)(g)
    }
  }
}
