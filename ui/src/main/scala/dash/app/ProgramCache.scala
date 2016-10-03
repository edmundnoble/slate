package dash
package app

import matryoshka.Fix
import qq.cc._
import qq.data.{ConcreteFilter, FilterComponent, JSON, Program}
import scodec.bits.BitVector
import Util._
import fastparse.core.{ParseError, Parsed}
import shapeless.ops.coproduct.Inject
import shapeless.{:+:, CNil, Coproduct}

import scalaz.\/
import scalaz.syntax.either._
import scalaz.syntax.applicative._
import scalaz.syntax.traverse._
import scalaz.std.list._

object ProgramCache {

  def prepareProgram(program: String): \/[Parsed.Failure, Program[Fix[FilterComponent]]] = {
    val parsedQQProgram = Parser.program.parse(program).toDisjunction.map(_.value)
    val optimizedProgram = parsedQQProgram.map(LocalOptimizer.optimizeProgram[Fix])
    optimizedProgram
  }

  case class InvalidBase64(str: String) extends Exception(str + " is not base64")
  case class InvalidBytecode(fail: scodec.Attempt.Failure) extends Exception("error decoding program from cache: " + fail.cause)

  type WhatCanGoWrong = QQCompilationException :+: ParseError :+: InvalidBase64 :+: InvalidBytecode :+: CNil

  def inj[C <: Coproduct, I](i: I)(implicit Inj: Inject[C, I]): C = Inj(i)

  // cache optimized, parsed programs using their hashcode as a key
  // store them as base64-encoded bytecode
  def getCachedCompiledProgram(qqProgram: String): StorageProgram[WhatCanGoWrong \/ CompiledFilter[JSON]] = {

    import StorageProgram._
    import qq.protocol.FilterProtocol._

    val hash = "program " + qqProgram.hashCode.toString
    for {
      programInStorage <- get(hash)
      decodedOptimizedProgram <- programInStorage match {
        case None =>
          val preparedProgram = prepareProgram(qqProgram)
            .leftMap(inj[WhatCanGoWrong, ParseError] _ compose ParseError.apply)
          val encodedProgram =
            preparedProgram.flatMap(
              programCodec
                .encode(_).toDisjunction
                .bimap(inj[WhatCanGoWrong, InvalidBytecode] _ compose InvalidBytecode, _.value)
            )
          val asBase64 = encodedProgram.map(_.toBase64)
          asBase64.fold(
            _.left[Program[ConcreteFilter]].pure[StorageProgram],
            update(hash, _).map(_ => preparedProgram)
          )
        case Some(encodedProgram) =>
          val encodedProgramBits =
            BitVector.fromBase64(encodedProgram)
              .fold(inj[WhatCanGoWrong, InvalidBase64](InvalidBase64(encodedProgram)).left[BitVector])(_.right)
          val decodedProgram =
            encodedProgramBits.flatMap(
              programCodec
                .decode(_).toDisjunction
                .bimap(inj[WhatCanGoWrong, InvalidBytecode] _ compose InvalidBytecode, _.value.value)
            )
          decodedProgram.pure[StorageProgram]
      }
      compiledProgram = decodedOptimizedProgram.flatMap(
        QQCompiler.compileProgram[Fix, JSON](JSONRuntime, DashPrelude, _).leftMap(inj[WhatCanGoWrong, QQCompilationException])
      )
    } yield compiledProgram
  }

}
