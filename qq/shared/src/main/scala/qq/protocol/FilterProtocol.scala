package qq.protocol

import qq.data._

object FilterProtocol {

  import scodec._
  import codecs._
  import CoproductBuilderAuto._
  import CoproductBuilderAutoDiscriminators._
  import scodec.codecs.implicits._
  import shapeless._

  type FilterComponentGenA[A] =
    CallFilter[A] :+: ComposeFilters[A] :+: ConstNumber[A] :+: ConstString[A] :+: Dereference[A] :+:
      EnjectFilters[A] :+: EnlistFilter[A] :+: EnsequenceFilters[A] :+: FilterMath[A] :+: LetAsBinding[A] :+: PathOperation[A] :+:
      SilenceExceptions[A] :+: CNil

  implicit def componentCodec[A](implicit v: Lazy[Codec[A]]): Codec[FilterComponent[A]] =
    ??? //    deriveGeneric(implicitly[Generic.Aux[FilterComponent[A], FilterComponentGenA[A]]], Lazy(Codec.coproduct[FilterComponentGenA[A]].auto))

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

  implicit def mathOperatorDiscriminated[A]: Discriminated[MathOperator, Int] = Discriminated(uint8)

  implicit val addDiscriminator: Discriminator[MathOperator, Add.type, Int] = Discriminator(0)
  implicit val subtractDiscriminator: Discriminator[MathOperator, Subtract.type, Int] = Discriminator(1)
  implicit val multiplyDiscriminator: Discriminator[MathOperator, Multiply.type, Int] = Discriminator(2)
  implicit val divideDiscriminator: Discriminator[MathOperator, Divide.type, Int] = Discriminator(3)
  implicit val moduloDiscriminator: Discriminator[MathOperator, Modulo.type, Int] = Discriminator(4)

  implicit def pathComponentDiscriminated[A]: Discriminated[PathComponent, Int] = Discriminated(uint8)

  implicit def collectDiscriminator: Discriminator[PathComponent, CollectResults.type, Int] = Discriminator(4)
  implicit def selectKeyDiscriminator: Discriminator[PathComponent, SelectKey, Int] = Discriminator(9)
  implicit def selectIndexDiscriminator: Discriminator[PathComponent, SelectIndex, Int] = Discriminator(10)
  implicit def selectRangeDiscriminator: Discriminator[PathComponent, SelectRange, Int] = Discriminator(11)

  implicit def pathOperationFDiscriminated[A]: Discriminated[PathOperationF[A], Int] = Discriminated(uint8)

  implicit def pathGetDiscriminator[A]: Discriminator[PathOperationF[A], PathGet[A], Int] = Discriminator(0)
  implicit def pathSetDiscriminator[A]: Discriminator[PathOperationF[A], PathSet[A], Int] = Discriminator(1)
  implicit def pathModifyDiscriminator[A]: Discriminator[PathOperationF[A], PathModify[A], Int] = Discriminator(2)

  implicit def filterComponentDiscriminated[A]: Discriminated[FilterComponentGenA[A], Int] = Discriminated(uint8)

  implicit def composeDiscriminator[A]: Discriminator[FilterComponentGenA[A], ComposeFilters[A], Int] = Discriminator(1)
  implicit def silenceDiscriminator[A]: Discriminator[FilterComponentGenA[A], SilenceExceptions[A], Int] = Discriminator(2)
  implicit def enlistDiscriminator[A]: Discriminator[FilterComponentGenA[A], EnlistFilter[A], Int] = Discriminator(3)
  implicit def ensequenceDiscriminator[A]: Discriminator[FilterComponentGenA[A], EnsequenceFilters[A], Int] = Discriminator(5)
  implicit def enjectDiscriminator[A]: Discriminator[FilterComponentGenA[A], EnjectFilters[A], Int] = Discriminator(6)
  implicit def callDiscriminator[A]: Discriminator[FilterComponentGenA[A], CallFilter[A], Int] = Discriminator(7)
  implicit def filterMathDiscriminator[A]: Discriminator[FilterComponentGenA[A], FilterMath[A], Int] = Discriminator(8)
  implicit def constNumberDiscriminator[A]: Discriminator[FilterComponentGenA[A], ConstNumber[A], Int] = Discriminator(12)
  implicit def constStringDiscriminator[A]: Discriminator[FilterComponentGenA[A], ConstString[A], Int] = Discriminator(13)
  implicit def letAsBindingDiscriminator[A]: Discriminator[FilterComponentGenA[A], LetAsBinding[A], Int] = Discriminator(14)
  implicit def dereferenceDiscriminator[A]: Discriminator[FilterComponentGenA[A], Dereference[A], Int] = Discriminator(15)

}
