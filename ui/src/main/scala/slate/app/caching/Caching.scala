package slate
package app
package caching

import cats.Monad
import cats.implicits._
import shapeless.{:+:, CNil}
import slate.storage.Storage

// domain-specific logic for caching in a Storage[F] based kv-store
object Caching {

  def getCachedBy[F[_] : Monad, ErrS, I, A](input: I)(getKey: I => String, decode: String => ErrS Either A)
                                           (implicit storage: Storage[F]): F[Option[ErrS Either A]] = {
    val key = getKey(input)
    storage(key).map(_.map(decode))
  }

  def getCachedByPrepare[F[_] : Monad, ErrS, ErrP, A, I](input: I)(
    getKey: I => String,
    encode: A => ErrS Either String,
    decode: (String, String) => ErrS Either A,
    prepare: I => ErrP Either A)(implicit storage: Storage[F]): F[(ErrP :+: ErrS :+: CNil) Either A] = {

    val injectError = inj[ErrP :+: ErrS :+: CNil]
    val key = getKey(input)

    for {
      programInStorage <- storage(key)
      decodedOptimizedProgram <- programInStorage match {
        case None =>
          val preparedProgram =
            prepare(input).leftMap(injectError(_))
          val encodedProgram =
            preparedProgram.flatMap(
              encode(_).leftMap(injectError(_))
            )
          encodedProgram.fold(
            Left(_).pure[F],
            storage.update(key, _).as(preparedProgram)
          )
        case Some(encodedProgram) =>
          decode(key, encodedProgram).leftMap(injectError(_)).pure[F]
      }
    } yield decodedOptimizedProgram
  }


}
