package dash

import cats.{Functor, Monad}
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}
import cats.implicits._

object StorageFS {
  final def fsroot = "fsroot"
  final val directoryDelimiter = ","

  def enumerateKey(key: String): StorageProgram[Option[Array[String]]] = {
    import StorageProgram._
    for {
      dirListString <- get(key)
    } yield dirListString.map(_.split(directoryDelimiter))
  }

  def enumerate(path: Vector[String]): StorageProgram[Array[String]] = {
    import StorageProgram._
    for {
      roots <- enumerate(fsroot)
      ne <- get(root)
    }
  }

  def getHash[F[_]](underlying: Storage[F], path: Vector[String]): F[String] = {

  }

  def allFolders[F[_]](underlying: Storage[F], root: String): F[List[String]] = {

  }
}

sealed class StorageFS[F[_] : Monad](underlying: Storage[F], path: Vector[String]) extends Storage[F] {
  override def apply(key: String): F[Option[String]] = for {
    hash <- getHash(path)
    firstResult <- underlying(hash)
  } yield ??? // Task.eval(underlying(key))

  override def update(key: String, data: String): F[Unit] = (underlying.update(key, data))

  override def remove(key: String): F[Unit] = (underlying.remove(key))
}

