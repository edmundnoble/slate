package dash
package app

import qq.QQCompiler.{CompiledFilter, OrCompilationError}
import qq.cc.{LocalOptimizer, Parser}
import qq.data.{ConcreteFilter, Program}
import qq.jsc.JSRuntime
import qq.{Parser, QQCompiler}
import scodec.bits.BitVector

object DashApp {

  // cache optimized, parsed programs using their hashcode as a key
  // store them as base64-encoded bytecode
  def getCachedCompiledProgram(qqProgram: String): StorageProgram[OrCompilationError[CompiledFilter[Any]]] = {

    import StorageProgram._
    import qq.data.FilterProtocol._

    val hash = qqProgram.hashCode.toString
    for {
      programInStorage <- get(hash)
      decodedOptimizedProgram <- programInStorage match {
        case None =>
          val parsedQQProgram = Parser.program.parse(qqProgram).get.value
          val optimizedProgram = LocalOptimizer.optimizeProgram(parsedQQProgram)
          val encodedOptimizedProgram = programCodec.encode(optimizedProgram).require
          val out = encodedOptimizedProgram.toBase64
          update(hash, out).map(_ => optimizedProgram)
        case Some(encodedProgram) =>
          Util.pureFC[StorageAction, Program[ConcreteFilter]](programCodec.decode(BitVector.fromBase64(encodedProgram).get).require.value)
      }
    } yield QQCompiler.compileProgram(JSRuntime, prelude = DashPrelude.all, decodedOptimizedProgram)
  }

}
