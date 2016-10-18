package dash
package app

import matryoshka.Fix
import qq.cc._
import qq.data.{ConcreteFilter, FilterComponent, JSON, Program}
import scodec.bits.BitVector
import Util._
import fastparse.core.{ParseError, Parsed}
import qq.util.Recursion.RecursionEngine
import shapeless.ops.coproduct.Inject
import shapeless.{:+:, CNil, Coproduct}

import scalaz.{ReaderT, \/}
import scalaz.syntax.either._
import scalaz.syntax.applicative._
import scalaz.syntax.std.option._
import scalaz.std.list._

object ProgramCache {

  def prepareProgram(program: String)(implicit rec: RecursionEngine): Parsed.Failure \/ Program[Fix[FilterComponent]] = {
    val parsedQQProgram = Parser.program.parse(program).toDisjunction.map(_.value)
    val optimizedProgram = parsedQQProgram.map(LocalOptimizer.optimizeProgram[Fix])
    optimizedProgram
  }

  case class InvalidBase64(str: String) extends Exception(str + " is not base64")
  case class InvalidBytecode(fail: scodec.Attempt.Failure) extends Exception("error decoding program from cache: " + fail.cause)

  type WhatCanGoWrong = ParseError :+: InvalidBase64 :+: InvalidBytecode :+: CNil

  // cache optimized, parsed programs using their hashcode as a key
  // store them as base64-encoded bytecode
  def getCachedProgram(qqProgram: String)(implicit rec: RecursionEngine): Retargetable[StorageProgram, WhatCanGoWrong \/ Program[ConcreteFilter]] = {

    import StorageProgram._
    import qq.protocol.FilterProtocol._

    val injectError = inj[WhatCanGoWrong]

    for {
      hash <- ReaderT.ask[StorageProgram, String].map(_ + " " + qqProgram.hashCode.toString)
      programInStorage <- ReaderT.kleisli[StorageProgram, String, Option[String]](_ => get(hash))
      decodedOptimizedProgram <- programInStorage match {
        case None =>
          val preparedProgram = prepareProgram(qqProgram)
            .leftMap(e => injectError(ParseError(e)))
          val encodedProgram =
            preparedProgram.flatMap(
              programCodec
                .encode(_).toDisjunction
                .bimap(e => injectError(InvalidBytecode(e)), _.value)
            )
          val asBase64 = encodedProgram.map(_.toBase64)
          asBase64.fold(
            _.left.pure[Retargetable[StorageProgram, ?]],
            (s: String) =>
              ReaderT.kleisli[StorageProgram, String, WhatCanGoWrong \/ Program[ConcreteFilter]](_ => update(hash, s).map(_ => preparedProgram))
          )
        case Some(encodedProgram) =>
          val encodedProgramBits =
            BitVector.fromBase64(encodedProgram)
              .toRightDisjunction(injectError(InvalidBase64(encodedProgram)))
          val decodedProgram =
            encodedProgramBits.flatMap(
              programCodec
                .decode(_).toDisjunction
                .bimap(e => injectError(InvalidBytecode(e)), _.value.value)
            )
          decodedProgram.pure[Retargetable[StorageProgram, ?]]
      }
    } yield decodedOptimizedProgram
  }

}
