package qq
package protocol

import scodec.bits.BitVector
import scodec.{Attempt, Decoder, Err}

final class VectorCodec[A](codec: Decoder[A], limit: Option[Int] = None) extends Decoder[Vector[A]] {

  def decode(buffer: BitVector) =
    Decoder.decodeCollect[Vector, A](codec, limit)(buffer)

  override def toString = s"vector($codec)"

}

object VectorCodec {

  def vectorOfN[A](countCodec: Decoder[Int], valueCodec: Decoder[A]): Decoder[Vector[A]] =
    countCodec
      .flatMap { count => new VectorCodec(valueCodec, Some(count)).map(v => (count, v)) }
      .emap[Vector[A]] { case (cnt, xs) =>
      if (xs.size == cnt) Attempt.successful(xs)
      else Attempt.failure(Err(s"Insufficient number of elements: decoded ${xs.size} but should have decoded $cnt"))
    }

}
