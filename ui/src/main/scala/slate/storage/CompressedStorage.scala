package slate.storage

import org.atnos.eff.{Eff, IntoPoly}
import slate.bindings.LZString

import scala.util.Try

final case class CompressedStorage[F](underlying: Storage[F]) extends Storage[F] {
  override def apply(key: String): Eff[F, Option[String]] =
    underlying(key).map(
      _.flatMap(r =>
        Try(LZString.decompressFromUTF16(r)).toOption
      )
    )

  override def update(key: String, data: String): Eff[F, Unit] =
    underlying.update(key, LZString.compressToUTF16(data))

  override def remove(key: String): Eff[F, Unit] =
    underlying.remove(key)
}

final case class CompressedStorageI[F, FI](underlying: Storage[F])(implicit ev: IntoPoly[F, FI]) extends Storage[FI] {
  private val underInto: Storage[FI] = underlying.into[FI]
  override def apply(key: String): Eff[FI, Option[String]] =
    underInto(key).map(
      _.flatMap(r =>
        Try(LZString.decompressFromUTF16(r)).toOption
      )
    )

  override def update(key: String, data: String): Eff[FI, Unit] =
    underInto.update(key, LZString.compressToUTF16(data))

  override def remove(key: String): Eff[FI, Unit] =
    underInto.remove(key)
}
