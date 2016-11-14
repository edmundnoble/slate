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
import storage.{FixedStorageAction, StorageProgram}
import cats.data.Xor
import cats.implicits._
import slate.app.SlateApp.DashProgram

object ProgramCache {

  def parseAndOptimizeProgram(program: String)(implicit rec: RecursionEngine): ParseError Xor Program[ConcreteFilter] = {
    val parsedQQProgram = Parser.program.parse(program).toXor.bimap(ParseError(_), _.value)
    val optimizedProgram = parsedQQProgram.map(LocalOptimizer.optimizeProgram)
    optimizedProgram
  }

  sealed abstract class ProgramSerializationException(msg: String) extends Exception(msg)
  case class InvalidBase64(str: String) extends ProgramSerializationException(str + " is not base64")
  case class InvalidBytecode(fail: scodec.Attempt.Failure) extends ProgramSerializationException("error decoding program from cache: " + fail.cause)

  type ErrorGettingCachedProgram = ParseError :+: ProgramSerializationException :+: CNil

  def getCachedBy[ErrS, A](input: A)(
    getKey: A => String,
    decode: String => ErrS Xor A): StorageProgram[Option[ErrS Xor A]] = {
    val key = getKey(input)
    for {
      raw <- StorageProgram.get(key)
      decodedProgram = raw.map(decode)
    } yield decodedProgram
  }

  def getCachedByPrepareM[ErrS, ErrP, F[_] : Monad, A, I](input: I)(
    getKey: I => String,
    encode: A => ErrS Xor String,
    decode: (String, String) => ErrS Xor A,
    prepare: I => ErrP Xor F[A]): FixedStorageAction[F, (ErrP :+: ErrS :+: CNil) Xor A] = {

    val injectError = inj[ErrP :+: ErrS :+: CNil]
    val key = getKey(input)

    for {
      programInStorage <- FixedStorageAction.get[F](key)
      decodedOptimizedProgram <- programInStorage match {
        case None =>
          for {
            preparedProgram <- prepare(input)
              .map(FixedStorageAction.hpure[F, A])
              .leftMap(injectError(_))
              .sequence[FixedStorageAction[F, ?], A]
            encodedProgram: ((ErrP :+: ErrS :+: CNil) Xor String) = preparedProgram.flatMap(encode(_).leftMap(injectError(_)))
            result <- encodedProgram.fold(
              _.left[A].pure[FixedStorageAction[F, ?]],
              FixedStorageAction.update[F](key, _).map(_ => preparedProgram)
            )
          } yield result
        case Some(encodedProgram) =>
          decode(key, encodedProgram).leftMap(injectError(_)).traverse(FixedStorageAction.pure[F, A])
      }
    } yield decodedOptimizedProgram
  }

  def getCachedByPrepare[ErrS, ErrP, A, I](input: I)(
    getKey: I => String,
    encode: A => ErrS Xor String,
    decode: (String, String) => ErrS Xor A,
    prepare: I => ErrP Xor A): StorageProgram[(ErrP :+: ErrS :+: CNil) Xor A] = {

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
            _.left.pure[StorageProgram],
            StorageProgram.update(key, _).as(preparedProgram)
          )
        case Some(encodedProgram) =>
          decode(key, encodedProgram).leftMap(injectError(_)).pure[StorageProgram]
      }
    } yield decodedOptimizedProgram
  }


  // cache optimized, parsed programs using their hashcode as a key
  // store them as base64-encoded bytecode
  def getCachedProgramByHash(qqProgram: DashProgram[String])(implicit rec: RecursionEngine): StorageProgram[ErrorGettingCachedProgram Xor Program[ConcreteFilter]] = {

    import qq.protocol.FilterProtocol

    getCachedByPrepare[ProgramSerializationException, ParseError, Program[ConcreteFilter], DashProgram[String]](qqProgram)(
      prog => prog.title + prog.program.hashCode.toString, { (program: Program[ConcreteFilter]) =>
        FilterProtocol.programCodec
          .encode(program)
          .toXor
          .bimap(InvalidBytecode(_): ProgramSerializationException, _.value.toBase64)
      }, { (_, encodedProgram) =>
        BitVector.fromBase64(encodedProgram)
          .toRightXor(InvalidBase64(encodedProgram): ProgramSerializationException)
          .flatMap(
            FilterProtocol.programCodec.decode(_)
              .toXor.bimap(InvalidBytecode(_): ProgramSerializationException, _.value.value)
          )
      }, dashProgram => parseAndOptimizeProgram(dashProgram.program)
    )
  }

}
