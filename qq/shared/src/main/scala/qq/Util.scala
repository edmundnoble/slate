package qq

import scodec.{Codec, Decoder, Encoder}
import shapeless.Lazy

import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec
import scalaz.{Monad, \/, ~>}
import scalaz.syntax.either._

object Util {

  implicit val TailRecMonad = new Monad[TailRec] {
    override def point[A](a: => A): TailRec[A] = TailCalls.done(a)
    override def bind[A, B](fa: TailRec[A])(f: (A) => TailRec[B]): TailRec[B] = fa.flatMap(f)
  }

  def withPrefixes[A](xss: List[List[A]], ys: List[A]): List[List[A]] =
    for {xs <- xss; y <- ys} yield y :: xs

  def foldWithPrefixes[A](firstPrefix: List[A], nextPrefices: List[A]*): List[List[A]] =
    nextPrefices.foldLeft(firstPrefix :: Nil)(withPrefixes)

  def single: Option ~> Seq = new (Option ~> Seq) {
    def apply[A](op: Option[A]): Seq[A] =
      op match {
        case None => Vector.empty[A]
        case Some(v) => Vector.empty[A] :+ v
      }
  }

  import scodec._
  import scodec.bits._

  implicit def eitherCodec[E, A](implicit E: Lazy[Codec[E]], A: Lazy[Codec[A]]): Codec[E \/ A] = {
    val enc = Encoder.apply { (v: E \/ A) =>
      v.fold(e => E.value.encode(e).map(BitVector.one ++ _), a => A.value.encode(a).map(BitVector.zero ++ _))
    }
    val dec = Decoder.apply { (in: BitVector) =>
      if (in.head) {
        E.value.decode(in.tail).map(_.map(_.left[A]))
      } else {
        A.value.decode(in.tail).map(_.map(_.right[E]))
      }
    }
    Codec(enc, dec)
  }

}
