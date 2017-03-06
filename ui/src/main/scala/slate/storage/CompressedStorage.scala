package slate.storage

import cats.Monad
import cats.implicits._
import slate.bindings.LZString

import scala.util.Try

final case class CompressedStorage[F[_] : Monad](underlying: Storage[F]) extends Storage[F] {
  override def apply(key: String): F[Option[String]] =
    underlying(key).map(
      _.flatMap(r =>
        Try(LZString.decompressFromUTF16(r)).toOption
      )
    )

  override def update(key: String, data: String): F[Unit] =
    underlying.update(key, LZString.compressToUTF16(data))

  override def remove(key: String): F[Unit] =
    underlying.remove(key)
}

