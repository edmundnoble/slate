package slate
package app

import cats.Monad
import qq.cc._
import qq.data.{ConcreteFilter, Program}
import scodec.bits.BitVector
import slate.util.Util._
import fastparse.all.ParseError
import qq.util.Recursion.RecursionEngine
import shapeless.{:+:, CNil}
import storage.{StorageAction, StorageProgram}
import org.atnos.eff._
import Eff._
import syntax.all._
import cats.implicits._
import slate.app.SlateApp.SlateProgram

object ProgramCache {

  def parseAndOptimizeProgram(program: String)(implicit rec: RecursionEngine): ParseError Either Program[ConcreteFilter] = {
    val parsedQQProgram = Parser.program.parse(program).toEither.bimap(ParseError(_), _.value)
    val optimizedProgram = parsedQQProgram.map(LocalOptimizer.optimizeProgram)
    optimizedProgram
  }

  sealed abstract class ProgramSerializationException(msg: String) extends Exception(msg)
  case class InvalidBase64(str: String) extends ProgramSerializationException(str + " is not base64")
  case class InvalidBytecode(err: scodec.Err) extends ProgramSerializationException("error decoding program from cache: " + err)

  type ErrorGettingCachedProgram = ParseError :+: ProgramSerializationException :+: CNil

  def getCachedBy[R, ErrS, I, A](input: I)(
    getKey: I => String,
    decode: String => ErrS Either A)(implicit ev: MemberIn[StorageAction, R]): Eff[R, Option[ErrS Either A]] = {
    val key = getKey(input)
    for {
      raw <- StorageProgram.get[R](key)
      decodedProgram = raw.map(decode)
    } yield decodedProgram
  }

  def getCachedByPrepare[ErrS, ErrP, A, I](input: I)(
    getKey: I => String,
    encode: A => ErrS Either String,
    decode: (String, String) => ErrS Either A,
    prepare: I => ErrP Either A): StorageProgram[(ErrP :+: ErrS :+: CNil) Either A] = {

    val injectError = inj[ErrP :+: ErrS :+: CNil]
    val key = getKey(input)

    for {
      programInStorage <- StorageProgram.get(key)
      decodedOptimizedProgram <- programInStorage match {
        case None =>
          val preparedProgram =
            prepare(input).leftMap(injectError(_))
          val encodedProgram =
            preparedProgram.flatMap(
              encode(_).leftMap(injectError(_))
            )
          encodedProgram.fold(
            Left(_).pure[StorageProgram],
            StorageProgram.update(key, _).as(preparedProgram)
          )
        case Some(encodedProgram) =>
          decode(key, encodedProgram).leftMap(injectError(_)).pure[StorageProgram]
      }
    } yield decodedOptimizedProgram
  }


  // cache optimized, parsed programs using their hashcode as a key
  // store them as base64-encoded bytecode
  def getCachedProgramByHash(qqProgram: SlateProgram[String])(implicit rec: RecursionEngine): StorageProgram[ErrorGettingCachedProgram Either Program[ConcreteFilter]] = {

    import qq.protocol.FilterProtocol

    getCachedByPrepare[ProgramSerializationException, ParseError, Program[ConcreteFilter], SlateProgram[String]](qqProgram)(
      prog => prog.title + prog.program.hashCode.toString, { (program: Program[ConcreteFilter]) =>
        FilterProtocol.programCodec
          .encode(program)
          .toEither
          .bimap(e => InvalidBytecode(e): ProgramSerializationException, _.toBase64)
      }, { (_, encodedProgram) =>
        BitVector.fromBase64(encodedProgram)
          .toRight(InvalidBase64(encodedProgram): ProgramSerializationException)
          .flatMap(
            FilterProtocol.programCodec.decode(_)
              .toEither.bimap(e => InvalidBytecode(e): ProgramSerializationException, _.value)
          )
      }, dashProgram => parseAndOptimizeProgram(dashProgram.program)
    )
  }

}
