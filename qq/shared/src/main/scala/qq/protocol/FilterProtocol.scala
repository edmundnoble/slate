package qq
package protocol

import qq.data._

import scodec._
import codecs._
import CoproductBuilderAuto._
import CoproductBuilderAutoDiscriminators._
import qq.util._
import scodec.codecs.implicits._
import shapeless._



object FilterProtocol {

  implicit def componentCodec[A](implicit v: Lazy[Codec[A]]): Codec[FilterComponent[A]] =
    Codec.coproduct[FilterComponent[A]].auto

  def filterCodec: Codec[ConcreteFilter] = implicitly[Codec[ConcreteFilter]]

  def definitionsCodec: Codec[Program.Definitions[ConcreteFilter]] = implicitly[Codec[Program.Definitions[ConcreteFilter]]]

  def programCodec: Codec[Program[ConcreteFilter]] = implicitly[Codec[Program[ConcreteFilter]]]

  def deriveGeneric[A, Rec](implicit lgen: Generic.Aux[A, Rec], auto: Lazy[Codec[Rec]]): Codec[A] =
    auto.value.xmap(lgen.from, lgen.to)

  implicit def mathOpCodec: Codec[MathOperator] =
    Codec.coproduct[MathOperator].auto

  implicit def pathCodec: Codec[PathComponent] =
    Codec.coproduct[PathComponent].auto

  implicit def pathOpFCodec[A](implicit A: Lazy[Codec[A]]): Codec[PathOperationF[A]] =
    Codec.coproduct[PathOperationF[A]].auto

  implicit def mathOperatorDiscriminated[A]: Discriminated[MathOperator, Int] = Discriminated(uint(3))

  implicit val addDiscriminator: Discriminator[MathOperator, Add.type, Int] = Discriminator(0)
  implicit val subtractDiscriminator: Discriminator[MathOperator, Subtract.type, Int] = Discriminator(1)
  implicit val multiplyDiscriminator: Discriminator[MathOperator, Multiply.type, Int] = Discriminator(2)
  implicit val divideDiscriminator: Discriminator[MathOperator, Divide.type, Int] = Discriminator(3)
  implicit val moduloDiscriminator: Discriminator[MathOperator, Modulo.type, Int] = Discriminator(4)
  implicit val equalDiscriminator: Discriminator[MathOperator, Equal.type, Int] = Discriminator(5)
  implicit val lteDiscriminator: Discriminator[MathOperator, LTE.type, Int] = Discriminator(6)
  implicit val gteDiscriminator: Discriminator[MathOperator, GTE.type, Int] = Discriminator(7)
  implicit val lessThanDiscriminator: Discriminator[MathOperator, LessThan.type, Int] = Discriminator(8)
  implicit val greaterThanDiscriminator: Discriminator[MathOperator, GreaterThan.type, Int] = Discriminator(9)

  implicit def pathComponentDiscriminated[A]: Discriminated[PathComponent, Int] = Discriminated(uint(3))

  implicit def collectDiscriminator: Discriminator[PathComponent, CollectResults.type, Int] = Discriminator(0)
  implicit def selectKeyDiscriminator: Discriminator[PathComponent, SelectKey, Int] = Discriminator(1)
  implicit def selectIndexDiscriminator: Discriminator[PathComponent, SelectIndex, Int] = Discriminator(2)
  implicit def selectRangeDiscriminator: Discriminator[PathComponent, SelectRange, Int] = Discriminator(3)

  implicit def pathOperationFDiscriminated[A]: Discriminated[PathOperationF[A], Int] = Discriminated(uint(3))

  implicit def pathGetDiscriminator[A]: Discriminator[PathOperationF[A], PathGet.type, Int] = Discriminator(0)
  implicit def pathSetDiscriminator[A]: Discriminator[PathOperationF[A], PathSet[A], Int] = Discriminator(1)
  implicit def pathModifyDiscriminator[A]: Discriminator[PathOperationF[A], PathModify[A], Int] = Discriminator(2)

  implicit def filterComponentDiscriminated[A]: Discriminated[FilterComponent[A], Int] = Discriminated(uint(4))

  implicit def composeDiscriminator[A]: Discriminator[FilterComponent[A], ComposeFilters[A], Int] = Discriminator(1)
  implicit def silenceDiscriminator[A]: Discriminator[FilterComponent[A], SilenceExceptions[A], Int] = Discriminator(2)
  implicit def enlistDiscriminator[A]: Discriminator[FilterComponent[A], EnlistFilter[A], Int] = Discriminator(3)
  implicit def ensequenceDiscriminator[A]: Discriminator[FilterComponent[A], EnsequenceFilters[A], Int] = Discriminator(4)
  implicit def enjectDiscriminator[A]: Discriminator[FilterComponent[A], EnjectFilters[A], Int] = Discriminator(5)
  implicit def callDiscriminator[A]: Discriminator[FilterComponent[A], CallFilter[A], Int] = Discriminator(6)
  implicit def notDiscriminator[A]: Discriminator[FilterComponent[A], FilterNot[A], Int] = Discriminator(7)
  implicit def filterMathDiscriminator[A]: Discriminator[FilterComponent[A], FilterMath[A], Int] = Discriminator(8)
  implicit def constNumberDiscriminator[A]: Discriminator[FilterComponent[A], ConstNumber[A], Int] = Discriminator(9)
  implicit def constStringDiscriminator[A]: Discriminator[FilterComponent[A], ConstString[A], Int] = Discriminator(10)
  implicit def constBooleanDiscriminator[A]: Discriminator[FilterComponent[A], ConstBoolean[A], Int] = Discriminator(11)
  implicit def asBindingDiscriminator[A]: Discriminator[FilterComponent[A], AsBinding[A], Int] = Discriminator(12)
  implicit def dereferenceDiscriminator[A]: Discriminator[FilterComponent[A], Dereference[A], Int] = Discriminator(13)
  implicit def pathOperationDiscriminator[A]: Discriminator[FilterComponent[A], PathOperation[A], Int] = Discriminator(14)

}
