package qq
package protocol

import qq.ast._
import scodec.bits.BitVector
import scodec.codecs._
import FilterProtocol._

object QQEncoderRuntime extends QQRuntime[BitVector] {

  trait EncoderPathTypeRuntime extends PathTypeRuntime[BitVector] {
    override type P = BitVector
    override val collectResults: BitVector = pathComponent(collectDiscriminator)

    override def selectIndex(index: Int): BitVector = pathComponent(selectIndexDiscriminator) ++ int32.encode(index).require

    override def empty: BitVector = BitVector.empty

    override def selectKey(key: String): BitVector = pathComponent(selectKeyDiscriminator) ++ utf8_32.encode(key).require

    override def selectRange(start: Int, end: Int): BitVector = pathComponent(selectKeyDiscriminator) ++ int32.encode(start).require ++ int32.encode(end).require

    override def append(p1: BitVector, p2: BitVector): BitVector = p1 ++ p2

    final def pathComponent[A](disc: Discriminator[PathComponent, A, Int]) =
      pathComponentDiscriminated.codec.encode(disc.value).require

  }

  override val path: PathRuntime[BitVector] = new PathRuntime[BitVector] {
    override def set(c: BitVector): PathTypeRuntime[BitVector] = new EncoderPathTypeRuntime {
      override def ret(p: BitVector): BitVector =
        filter(pathOperationDiscriminator) ++ p ++ pathOp(pathSetDiscriminator) ++ c
    }

    override def modify(c: BitVector): PathTypeRuntime[BitVector] = new EncoderPathTypeRuntime {
      override def ret(p: BitVector): BitVector =
        filter(pathOperationDiscriminator) ++ p ++ pathOp(pathModifyDiscriminator) ++ c
    }

    override val get: PathTypeRuntime[BitVector] = new EncoderPathTypeRuntime {
      override def ret(p: BitVector): BitVector =
        filter(pathOperationDiscriminator) ++ p ++ pathOp(pathGetDiscriminator)
    }
  }

  final def filter[FC[_], A](disc: Discriminator[FilterComponent[A], FC[A], Int]) =
    filterComponentDiscriminated.codec.encode(disc.value).require

  final def pathOp[FC, A](disc: Discriminator[PathOperationF[A], FC, Int]) =
    pathOperationFDiscriminated.codec.encode(disc.value).require

  final def math[A](disc: Discriminator[MathOperator, A, Int], first: BitVector, second: BitVector) =
    filter(filterMathDiscriminator) ++ first ++ second ++ uint(3).encode(disc.value).require

  override def dereference(name: String): BitVector =
    filter(dereferenceDiscriminator) ++ utf8_32.encode(name).require

  override val filterNot: BitVector =
    filter(notDiscriminator)

  override def add(first: BitVector, second: BitVector): BitVector =
    math(addDiscriminator, first, second)

  override def subtract(first: BitVector, second: BitVector): BitVector =
    math(subtractDiscriminator, first, second)

  override def multiply(first: BitVector, second: BitVector): BitVector =
    math(multiplyDiscriminator, first, second)

  override def divide(first: BitVector, second: BitVector): BitVector =
    math(divideDiscriminator, first, second)

  override def modulo(first: BitVector, second: BitVector): BitVector =
    math(moduloDiscriminator, first, second)

  override def equal(first: BitVector, second: BitVector): BitVector =
    math(equalDiscriminator, first, second)

  override def lte(first: BitVector, second: BitVector): BitVector =
    math(lteDiscriminator, first, second)

  override def gte(first: BitVector, second: BitVector): BitVector =
    math(gteDiscriminator, first, second)

  override def lessThan(first: BitVector, second: BitVector): BitVector =
    math(lessThanDiscriminator, first, second)

  override def greaterThan(first: BitVector, second: BitVector): BitVector =
    math(greaterThanDiscriminator, first, second)

  override def silenceExceptions(f: BitVector): BitVector =
    filter(silenceDiscriminator) ++ f

  override def constNumber(num: Double): BitVector =
    filter(constNumberDiscriminator) ++ doubleL.encode(num).require

  override def constString(str: String): BitVector =
    filter(constStringDiscriminator) ++ utf8_32.encode(str).require

  override def constBoolean(boo: Boolean): BitVector =
    filter(constBooleanDiscriminator) ++ bool.encode(boo).require

  override def enlistFilter(inside: BitVector): BitVector =
    filter(enlistDiscriminator) ++ inside

  // TODO: is this reversible
  override def enject(obj: Vector[(Either[String, BitVector], BitVector)]): BitVector =
    filter(enjectDiscriminator) ++ vector(either(bool, utf8_32, bits) ~ bits).encode(obj).require

  override def asBinding(name: String, as: BitVector, in: BitVector): BitVector =
    filter(asBindingDiscriminator) ++ utf8_32.encode(name).require ++ as ++ in

  override def ensequence(first: BitVector, second: BitVector): BitVector =
    filter(ensequenceDiscriminator) ++ first ++ second

  override def composeFilters(f: BitVector, s: BitVector): BitVector =
    filter(composeDiscriminator) ++ f ++ s

}
