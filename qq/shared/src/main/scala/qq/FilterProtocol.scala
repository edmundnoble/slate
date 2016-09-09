package qq

object FilterProtocol {

  import scodec._
  import scodec.bits._
  import codecs._
  import scalaz.syntax.either._
  import Util._

  import scodec.codecs.implicits._
  import CoproductBuilderAuto._
  import CoproductBuilderAutoDiscriminators._
  import shapeless._

  type FilterComponentGenA[A] =
    AddFilters[A] :+: CallFilter[A] :+: CollectResults[A] :+: ComposeFilters[A] :+:
      ConstNumber[A] :+: ConstString[A] :+: Dereference[A] :+: DivideFilters[A] :+:
      EnjectFilters[A] :+: EnlistFilter[A] :+: EnsequenceFilters[A] :+: IdFilter[A] :+: LetAsBinding[A] :+:
      ModuloFilters[A] :+: MultiplyFilters[A] :+:
      SelectIndex[A] :+: SelectKey[A] :+: SelectRange[A] :+: SilenceExceptions[A] :+: SubtractFilters[A] :+: CNil

  implicit def componentCodec[A](implicit v: Lazy[Codec[A]]): Codec[FilterComponent[A]] =
    deriveGeneric[FilterComponent[A], FilterComponentGenA[A]](Generic.apply[FilterComponent[A]], Lazy(Codec.coproduct[FilterComponentGenA[A]].auto))

  def filterCodec: Codec[Filter] = implicitly
  def programCodec: Codec[Program] = implicitly

  def deriveGeneric[A, Rec](implicit lgen: Generic.Aux[A, Rec], auto: Lazy[Codec[Rec]]): Codec[A] =
    auto.value.xmap(lgen.from, lgen.to)

  implicit def discriminated[A]: Discriminated[FilterComponentGenA[A], Int] = Discriminated(uint8)
  implicit def idDiscriminator[A]: Discriminator[FilterComponentGenA[A], IdFilter[A], Int] = Discriminator(0)
  implicit def composeDiscriminator[A]: Discriminator[FilterComponentGenA[A], ComposeFilters[A], Int] = Discriminator(1)
  implicit def silenceDiscriminator[A]: Discriminator[FilterComponentGenA[A], SilenceExceptions[A], Int] = Discriminator(2)
  implicit def enlistDiscriminator[A]: Discriminator[FilterComponentGenA[A], EnlistFilter[A], Int] = Discriminator(3)
  implicit def collectDiscriminator[A]: Discriminator[FilterComponentGenA[A], CollectResults[A], Int] = Discriminator(4)
  implicit def ensequenceDiscriminator[A]: Discriminator[FilterComponentGenA[A], EnsequenceFilters[A], Int] = Discriminator(5)
  implicit def enjectDiscriminator[A]: Discriminator[FilterComponentGenA[A], EnjectFilters[A], Int] = Discriminator(6)
  implicit def callDiscriminator[A]: Discriminator[FilterComponentGenA[A], CallFilter[A], Int] = Discriminator(7)
  //noinspection MutatorLikeMethodIsParameterless
  implicit def addDiscriminator[A]: Discriminator[FilterComponentGenA[A], AddFilters[A], Int] = Discriminator(8)
  implicit def subtractDiscriminator[A]: Discriminator[FilterComponentGenA[A], SubtractFilters[A], Int] = Discriminator(9)
  implicit def multiplyDiscriminator[A]: Discriminator[FilterComponentGenA[A], MultiplyFilters[A], Int] = Discriminator(10)
  implicit def divideDiscriminator[A]: Discriminator[FilterComponentGenA[A], DivideFilters[A], Int] = Discriminator(11)
  implicit def moduloDiscriminator[A]: Discriminator[FilterComponentGenA[A], ModuloFilters[A], Int] = Discriminator(12)
  implicit def selectKeyDiscriminator[A]: Discriminator[FilterComponentGenA[A], SelectKey[A], Int] = Discriminator(13)
  implicit def selectIndexDiscriminator[A]: Discriminator[FilterComponentGenA[A], SelectIndex[A], Int] = Discriminator(14)
  implicit def selectRangeDiscriminator[A]: Discriminator[FilterComponentGenA[A], SelectRange[A], Int] = Discriminator(15)
  implicit def constNumberDiscriminator[A]: Discriminator[FilterComponentGenA[A], ConstNumber[A], Int] = Discriminator(16)
  implicit def constStringDiscriminator[A]: Discriminator[FilterComponentGenA[A], ConstString[A], Int] = Discriminator(17)
  implicit def letAsBindingDiscriminator[A]: Discriminator[FilterComponentGenA[A], LetAsBinding[A], Int] = Discriminator(18)
  implicit def dereferenceDiscriminator[A]: Discriminator[FilterComponentGenA[A], Dereference[A], Int] = Discriminator(19)

}
