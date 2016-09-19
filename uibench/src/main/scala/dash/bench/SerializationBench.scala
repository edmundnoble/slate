package dash
package bench

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import matryoshka._
import qq.data.{ConcreteFilter, QQDSL, FilterProtocol}
import scodec.bits.BitVector

object SerializationBench {

  val selectKeyBuilder: Benchmark.Builder[Int, ConcreteFilter] =
    Benchmark.setup[Int, ConcreteFilter](i => Util.composeBuildRec[Fix](i, QQDSL.fix.selectKey("key")))
  val idBuilder: Benchmark.Builder[Int, ConcreteFilter] =
    Benchmark.setup[Int, ConcreteFilter](i => Util.composeBuildRec[Fix](i, QQDSL.fix.id))

  def preEncode(b: Benchmark.Builder[Int, ConcreteFilter]): Benchmark.Builder[Int, BitVector] =
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
