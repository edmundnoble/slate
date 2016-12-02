package slate
package app
package caching

import cats.implicits._
import org.atnos.eff.Eff._
import org.atnos.eff._, syntax.all._
import shapeless.{:+:, CNil}
import slate.storage.StorageAction._storageAction
import slate.storage.StorageProgram

// domain-specific logic for caching in a Storage[F] based kv-store
object Caching {

  def getCachedBy[R: _storageAction, ErrS, I, A](input: I)(getKey: I => String, decode: String => ErrS Either A): Eff[R, Option[ErrS Either A]] = {
    StorageProgram.get[R](getKey(input)).map(_.map(decode))
  }

  def getCachedByPrepare[R: _storageAction, ErrS, ErrP, A, I](input: I)(
    getKey: I => String,
    encode: A => ErrS Either String,
    decode: (String, String) => ErrS Either A,
    prepare: I => ErrP Either A): Eff[R, (ErrP :+: ErrS :+: CNil) Either A] = {

    val injectError = copInj[ErrP :+: ErrS :+: CNil]
    val key = getKey(input)

    for {
      programInStorage <- StorageProgram.get[R](key)
      decodedOptimizedProgram <- programInStorage match {
        case None =>
          val preparedProgram =
            prepare(input).leftMap(injectError(_))
          val encodedProgram =
            preparedProgram.flatMap(
              encode(_).leftMap(injectError(_))
            )
          encodedProgram.fold(
            Left(_).pureEff[R],
            StorageProgram.update[R](key, _).as(preparedProgram)
          )
        case Some(encodedProgram) =>
          decode(key, encodedProgram).leftMap(injectError(_)).pureEff[R]
      }
    } yield decodedOptimizedProgram
  }


}
