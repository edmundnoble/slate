package qq
package protocol

import cats._
import cats.implicits.{toFlatMapOps => _, _}
import org.atnos.eff.{either => eitherEff, _}
import org.atnos.eff.syntax.all.{toEffOneEffectOps => _, _}
import qq.ast.{PathTypeRuntime, QQRuntime}
import qq.cc.CompileError.OrCompileError
import qq.cc.{CompileError, DefinitionMap, Prelude, QQCompiler, WrongNumParams}
import qq.data.CompiledDefinition
import scodec.{Attempt, DecodeResult, Decoder}
import scodec.codecs._
import FilterProtocol._
import qq.cc.QQCompiler.PathParserE
import scodec.Attempt.{Failure, Successful}
import scodec.bits.BitVector

import scala.annotation.{switch, tailrec}

final class QQDecoderCompiler[C0](val runtime: QQRuntime[C0], val prelude: Prelude[C0]) extends QQCompiler[Decoder] {
  override type C = C0

  override implicit val func: Monad[Decoder] = new Monad[Decoder] {
    override def pure[A](x: A): Decoder[A] = Decoder.liftAttempt(Attempt.successful(x))

    override def flatMap[A, B](fa: Decoder[A])(f: (A) => Decoder[B]): Decoder[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: (A) => Decoder[Either[A, B]]): Decoder[B] = (bits: BitVector) => {
      @tailrec
      def loop(a: A, bits: BitVector): Attempt[DecodeResult[B]] = {
        f(a).decode(bits) match {
          case Failure(e) => Failure(e)
          case Successful(r) => r.value match {
            case Left(aa) => loop(aa, r.remainder)
            case Right(b) => Successful(DecodeResult(b, r.remainder))
          }
        }
      }

      loop(a, bits)
    }
  }

  val decoderEffApp: Applicative[({
    type λ[α] = Decoder[Eff[S, α]]
  })#λ] = func.compose(Eff.EffApplicative[S])

  trait PathTypeRuntimeConsumer {
    def apply(prt: PathTypeRuntime[C]): prt.P
  }

  @tailrec
  def makeDecodeFolder[X](limit: Int, decoder: Decoder[X], acc: Decoder[X])(fold: (X, X) => X): Decoder[X] =
    if (limit == 0) acc
    else makeDecodeFolder(limit - 1, decoder, func.map2(acc, decoder)((i, d) => fold(i, d)))(fold)

  def mapFold[A, B](vec: Vector[A])(init: B, inj: A => B, agg: (B, B) => B): B =
    vec.foldLeft(init)((b, a) => agg(b, inj(a)))

  val filter: Decoder[Eff[S, C0]] = func.flatMap(filterComponentDiscriminated.codec.asDecoder) { fcidx =>
    type meme = Member.Aux[OrCompileError, S, V]
    (fcidx: @switch) match {
      case 0 => decoderEffApp.map2(filter, filter)(runtime.composeFilters)
      case 1 => decoderEffApp.map(filter)(runtime.silenceExceptions)
      case 2 => decoderEffApp.map(filter)(runtime.enlistFilter)
      case 3 => decoderEffApp.map2(filter, filter)(runtime.ensequence)
      case 4 => VectorCodec.vectorOfN(int16,
        bool.asDecoder
          .flatMap { b => if (b) utf8_32.asDecoder.map(k => k.asLeft[C].pureEff[S]) else filter.map(_.map(_.asRight[String])) }
          .flatMap(b => filter.map(f => (b, f)))
      ).map(ps => ps.traverseA(t => Eff.EffApplicative[S].tuple2(t._1, t._2)).map(runtime.enject))
      case 5 => func.map2(utf8_32.asDecoder, VectorCodec.vectorOfN(int16, filter))(
        (s, ps) =>
          for {
            params <- Eff.sequenceA[S, Vector, C0](ps)
            defs <- reader.ask[S, DefinitionMap[C]]
            called <- eitherEff.fromEither[S, CompileError, C](QQCompiler.callFilter[C](defs, s, params))
          } yield called
      )
      case 6 => decoderEffApp.pure(runtime.filterNot)
      case 7 => decoderEffApp.map3(filter, filter, mathOperatorDiscriminated.codec.asDecoder.map(Eff.pure[S, Int]))((f, s, o) => (o: @switch) match {
        case 0 => runtime.add(f, s)
        case 1 => runtime.subtract(f, s)
        case 2 => runtime.multiply(f, s)
        case 3 => runtime.divide(f, s)
        case 4 => runtime.modulo(f, s)
        case 5 => runtime.equal(f, s)
        case 6 => runtime.lte(f, s)
        case 7 => runtime.gte(f, s)
        case 8 => runtime.lessThan(f, s)
        case 9 => runtime.greaterThan(f, s)
      })
      case 8 => double.map(runtime.constNumber(_).pureEff[S])
      case 9 => utf8_32.map(runtime.constString(_).pureEff[S])
      case 10 => bool.map(runtime.constBoolean(_).pureEff[S])
      case 11 => decoderEffApp.map3(utf8_32.map(_.pureEff[S]), filter, filter)(runtime.asBinding)
      case 12 => utf8_32.map(runtime.dereference(_).pureEff[S])
      case 13 =>
        val ret = func.map2(int16, int16.flatMap(pathopidx => (pathopidx: @switch) match {
          case 0 => runtime.path.get.pureEff[S].pure[Decoder]
          case 1 => filter.map(_.map(runtime.path.set))
          case 2 => filter.map(_.map(runtime.path.modify))
        })) { (i, prtE) =>
          val ret: Decoder[Vector[PathParserE]] =
            makeDecodeFolder[Vector[PathParserE]](i, pathComponentDiscriminated.codec.asDecoder.flatMap { pathcmpidx =>
              (pathcmpidx: @switch) match {
                case 0 => func.pure(Vector(PathParserE.collectResults))
                case 1 => utf8_32.map(k => Vector(PathParserE.selectKey(k)))
                case 2 => int32.map(i => Vector(PathParserE.selectIndex(i)))
                case 3 => func.map2(int32, int32)((s, e) => Vector(PathParserE.selectRange(s, e)))
              }
            }, func.pure(Vector.empty))(_ ++ _)
          ret.tupleRight(prtE)
        }.flatten.map { x =>
          val (es, prtE) = x
          for {
            prt <- prtE
            folded = prt.ret(mapFold[PathParserE, prt.P](es)(prt.empty, _.apply(prt), prt.append(_, _)))
          } yield folded
        }
        ret
    }
  }

  val prtParser: Decoder[Eff[S, PathTypeRuntime[C0]]] =
    int16.flatMap(pathcompidx => (pathcompidx: @switch) match {
      case 0 => runtime.path.get.pureEff[S].pure[Decoder]
      case 1 => filter.map(_.map(runtime.path.set))
      case 2 => filter.map(_.map(runtime.path.modify))
    })
  val definition: Decoder[Eff[S, CompiledDefinition[C0]]] =
    for {
      name <- utf8_32.asDecoder
      args <- VectorCodec.vectorOfN(int16, utf8_32.asDecoder)
      f <- filter
    } yield reader.ask[S, DefinitionMap[C]].map(defns =>
      CompiledDefinition[C](name, args.length, { (muhcs) =>
        if (args.length != muhcs.length) {
          Either.left[CompileError, C](
            WrongNumParams(name, args.length, muhcs.length)
          )
        } else {
          val paramDefinitions = (args, muhcs).zipped.map { (filterName, value) =>
            CompiledDefinition(filterName, 0, (_: Vector[C]) => Right(value))
          }(collection.breakOut)
          Eff.detach(
            reader.runReader[S, E, DefinitionMap[C], C](
              defns ++ paramDefinitions.map(d => d.name -> d)(collection.breakOut)
            )(f)
          )
        }
      })
    )
  override val definitions: Decoder[Vector[Eff[S, CompiledDefinition[C0]]]] =
    VectorCodec.vectorOfN(int16, definition)
  override val program: Decoder[OrCompileError[C0]] =
    for {
      defs <- definitionMap
      filt <- filter.map(f => defs.flatMap(ds => Eff.detach(reader.runReader[S, E, DefinitionMap[C], C](ds)(f))))
    } yield filt
}
