package slate
package app
package caching

import cats.implicits._
import org.atnos.eff.Eff._
import org.atnos.eff._
import qq.util.Recursion.RecursionEngine
import shapeless.{:+:, CNil}
import slate.storage.{StorageAction, StorageProgram}

// domain-specific logic for caching in a Storage[F] based kv-store
object Caching {

  def getCachedBy[R, ErrS, I, A](input: I)(getKey: I => String, decode: String => ErrS Either A)(
    implicit ev: MemberIn[StorageAction, R]): Eff[R, Option[ErrS Either A]] = {
    val key = getKey(input)
    StorageProgram.get[R](key).map(_.map(decode))
  }

  def getCachedByPrepare[ErrS, ErrP, A, I](input: I)(
    getKey: I => String,
    encode: A => ErrS Either String,
    decode: (String, String) => ErrS Either A,
    prepare: I => ErrP Either A): StorageProgram[(ErrP :+: ErrS :+: CNil) Either A] = {

    val injectError = inj[ErrP :+: ErrS :+: CNil]
    val key = getKey(input)

    for {
      programInStorage <- StorageProgram.get(key)
      decodedOptimizedProgram <- programInStorage match {
        case None =>
          val preparedProgram =
            prepare(input).leftMap(injectError(_))
          val encodedProgram =
            preparedProgram.flatMap(
              encode(_).leftMap(injectError(_))
            )
          encodedProgram.fold(
            Left(_).pure[StorageProgram],
            StorageProgram.update(key, _).as(preparedProgram)
          )
        case Some(encodedProgram) =>
          decode(key, encodedProgram).leftMap(injectError(_)).pure[StorageProgram]
      }
    } yield decodedOptimizedProgram
  }


}
