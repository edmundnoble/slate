package dash
package app

import qq.{Optimizer, Parser, Program, QQCompiler}
import qq.QQCompiler.{CompiledFilter, OrCompilationError}
import scodec.bits.BitVector

import qq.jsc.JSRuntime

object DashApp {

  // cache optimized, parsed programs using their hashcode as a key
  // store them as base64-encoded bytecode
  def getCachedCompiledProgram(qqProgram: String): StorageProgram[OrCompilationError[CompiledFilter[Any]]] = {

    import qq.FilterProtocol._
    import StorageProgram._

    val hash = qqProgram.hashCode.toString
    for {
      programInStorage <- get(hash)
      decodedOptimizedProgram <- programInStorage match {
        case None =>
          val parsedQQProgram = Parser.program.parse(qqProgram).get.value
          val optimizedProgram = Optimizer.optimizeProgram(parsedQQProgram)
          val encodedOptimizedProgram = programCodec.encode(optimizedProgram).require
          val out = encodedOptimizedProgram.toBase64
          update(hash, out).map(_ => optimizedProgram)
        case Some(encodedProgram) =>
          Util.pureFC[StorageAction, Program](programCodec.decode(BitVector.fromBase64(encodedProgram).get).require.value)
      }
    } yield QQCompiler.compileProgram(JSRuntime, prelude = DashPrelude.all, decodedOptimizedProgram)
  }

}
