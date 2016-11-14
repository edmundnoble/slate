package slate
package app

import qq.cc._
import qq.data.{ConcreteFilter, Program}
import scodec.bits.BitVector
import slate.util.Util._
import fastparse.all.ParseError
import qq.util.Recursion.RecursionEngine
import shapeless.{:+:, CNil}

import cats.data.Xor
import cats.implicits._

object ProgramCache {

  def prepareProgram(program: String)(implicit rec: RecursionEngine): ParseError Xor Program[ConcreteFilter] = {
    val parsedQQProgram = Parser.program.parse(program).toXor.bimap(ParseError(_), _.value)
    val optimizedProgram = parsedQQProgram.map(LocalOptimizer.optimizeProgram)
    optimizedProgram
  }

  sealed abstract class ProgramSerializationError(msg: String) extends Exception(msg)
  case class InvalidBase64(str: String) extends ProgramSerializationError(str + " is not base64")
  case class InvalidBytecode(fail: scodec.Attempt.Failure) extends ProgramSerializationError("error decoding program from cache: " + fail.cause)

  type ErrorGettingCachedProgram = ParseError :+: ProgramSerializationError :+: CNil

  def getCachedBy[ErrS, ErrP, A, I](input: I)(
    getKey: I => String,
    encode: A => ErrS Xor String,
    decode: String => ErrS Xor A,
    prepare: I => ErrP Xor A)(implicit rec: RecursionEngine): StorageProgram[(ErrP :+: ErrS :+: CNil) Xor A] = {

    import storage.StorageProgram._

    val injectError = inj[ErrP :+: ErrS :+: CNil]
    val key = getKey(input)

    for {
      programInStorage <- get(key)
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
            update(key, _).as(preparedProgram)
          )
        case Some(encodedProgram) =>
          decode(encodedProgram)
            .leftMap(injectError(_))
            .pure[StorageProgram]
      }
    } yield decodedOptimizedProgram
  }


  // cache optimized, parsed programs using their hashcode as a key
  // store them as base64-encoded bytecode
  def getCachedProgramByHash(qqProgram: String)(implicit rec: RecursionEngine): StorageProgram[ErrorGettingCachedProgram Xor Program[ConcreteFilter]] = {

    import qq.protocol.FilterProtocol._

    getCachedBy(qqProgram)(
      _.hashCode.toString, { program =>
        programCodec
          .encode(program)
          .toXor
          .bimap(InvalidBytecode(_): ProgramSerializationError, _.value.toBase64)
      }, { encodedProgram =>
        val deBased = BitVector.fromBase64(encodedProgram)
          .toRightXor(InvalidBase64(encodedProgram): ProgramSerializationError)
        deBased.flatMap(
          programCodec.decode(_)
            .toXor.bimap(InvalidBytecode(_): ProgramSerializationError, _.value.value)
        )
      }, prepareProgram
    )
  }

}
