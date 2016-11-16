package slate
package app
package caching

import qq.cc.LocalOptimizer
import cats.Monad
import qq.cc._
import qq.data
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

object Program {

  def parseAndOptimizeProgram(program: String)(implicit rec: RecursionEngine): ParseError Either data.Program[data.FilterAST] = {
    val parsedQQProgram = Parser.program.parse(program).toEither.bimap(ParseError(_), _.value)
    val optimizedProgram = parsedQQProgram.map(LocalOptimizer.optimizeProgram)
    optimizedProgram
  }

  sealed abstract class ProgramSerializationException(msg: String) extends Exception(msg)
  class InvalidBase64(str: String) extends ProgramSerializationException(str + " is not base64")
  object InvalidBase64 {
    def apply(str: String): ProgramSerializationException = new InvalidBase64(str)
  }
  class InvalidBytecode(err: scodec.Err) extends ProgramSerializationException("error decoding program from cache: " + err)
  object InvalidBytecode {
    def apply(err: scodec.Err): ProgramSerializationException = new InvalidBytecode(err)
  }

  type ErrorGettingCachedProgram = ParseError :+: ProgramSerializationException :+: CNil

  // cache optimized, parsed programs using their hashcode as a key
  // store them as base64-encoded bytecode
  def getCachedProgramByHash(qqProgram: SlateProgram[String])(implicit rec: RecursionEngine): StorageProgram[ErrorGettingCachedProgram Either data.Program[data.FilterAST]] = {

    import qq.protocol.FilterProtocol

    Caching.getCachedByPrepare[ProgramSerializationException, ParseError, data.Program[data.FilterAST], SlateProgram[String]](qqProgram)(
      prog => prog.title + prog.program.hashCode.toString, { (program: data.Program[data.FilterAST]) =>
        FilterProtocol.programCodec
          .encode(program)
          .toEither
          .bimap(InvalidBytecode(_), _.toBase64)
      }, { (_, encodedProgram) =>
        BitVector.fromBase64(encodedProgram)
          .toRight(InvalidBase64(encodedProgram))
          .flatMap(
            FilterProtocol.programCodec.decode(_)
              .toEither.bimap(InvalidBytecode(_): ProgramSerializationException, _.value)
          )
      }, dashProgram => parseAndOptimizeProgram(dashProgram.program)
    )
  }

}
