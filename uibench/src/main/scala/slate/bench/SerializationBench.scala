package slate
package bench

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import qq.data.{FilterAST, QQDSL}
import qq.protocol.FilterProtocol
import scodec.bits.BitVector

object SerializationBench {

  val selectKeyBuilder: Benchmark.Builder[Int, FilterAST] =
    Benchmark.setup[Int, FilterAST](i => Util.composeBuildRec(i, QQDSL.getPathS(QQDSL.selectKey("key"))))
  val idBuilder: Benchmark.Builder[Int, FilterAST] =
    Benchmark.setup[Int, FilterAST](i => Util.composeBuildRec(i, QQDSL.id))

  def preEncode(b: Benchmark.Builder[Int, FilterAST]): Benchmark.Builder[Int, BitVector] =
    new Benchmark.Builder[Int, BitVector](b.prepare.andThen(FilterProtocol.filterCodec.encode).andThen(_.require))

  val serializationBenchSuite: GuiSuite[Int] = GuiSuite(
    Suite("QQ Program Serialization Benchmarks")(
      selectKeyBuilder("encode fix select key")(FilterProtocol.filterCodec.encode(_).require),
      idBuilder("encode fix compose with id")(FilterProtocol.filterCodec.encode(_).require),
      preEncode(selectKeyBuilder)("decode fix select key")(FilterProtocol.filterCodec.decode(_).require.value),
      preEncode(idBuilder)("decode fix compose with id")(FilterProtocol.filterCodec.decode(_).require.value)
    ), GuiParams.one(GuiParam.int("filter size", 5, 10, 50))
  )
}
