package dash

import java.util.Date

import cats.data.Xor
import cats.free.FreeT
import cats.{Functor, Monad}
import scalajs.js
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}
import cats.implicits._

object StorageFS {
  final def fsroot = "fsroot"
  // TODO: static safety for escaping these
  final val keyDelimiter = " "
  final val metadataDelimiter = " "
  final val interMetadataDelimiter = ":"
  final val nodeKindDelimiter = ":"
  final val interNodeDelimiter = ","

  final case class StorageKey[F](name: String, nonce: String)
  case class FileData(data: String)

  final case class NodeMetadata(lastUpdated: js.Date, lastAccessed: js.Date)
  case class File(metadata: NodeMetadata, key: StorageKey[File], dataHash: String, dataKey: StorageKey[FileData])
  case class Dir(metadata: NodeMetadata, key: StorageKey[Dir], childKeys: List[StorageKey[File] Xor StorageKey[Dir]])
  type FSKey = StorageKey[File] Xor StorageKey[Dir]
  type FSEntry = File Xor Dir

  def getRaw(key: StorageKey[_]): StorageProgram[Option[String]] = {
    import StorageProgram._
    for {
      raw <- get(key.name + keyDelimiter + key.nonce)
    } yield raw
  }

  def readMetadata(metadataStr: String): Option[NodeMetadata] = {
    val (lastUpdated: js.Date, lastAccessed: js.Date) = {
      val splat = metadataStr.split(interMetadataDelimiter)
      val dates = splat.map(s => new js.Date(js.Date.parse(s)))
      (dates(0), dates(1))
    }
    NodeMetadata(lastUpdated, lastAccessed)

  }

  // dir format:
  // metadata (last updated and last accessed datetime) separated by interMetadataDelimiter
  // then metadataDelimiter
  // then file keys separated from folder keys by nodeKindDelimiter, where keys consist of
  // node names, keyDelimiter, node nonces
  // and keys are separated from each other by interNodeDelimiter

  def getDir(key: StorageKey[Dir]): StorageProgram[Option[Dir]] = {
    (for {
      rawMaybe <- getRaw(key)
    } yield rawMaybe.map { raw =>
      val metadataIndex = raw.indexOf(metadataDelimiter)
      val metadataStr = raw.substring(0, metadataIndex)
      val dataStr = raw.substring(metadataIndex)
      for {
        metadata <- readMetadata(metadataStr)
        allKeys <- {
          val splatKinds = dataStr.split(nodeKindDelimiter).map(_.split(interNodeDelimiter).map(_.split(keyDelimiter)))
          if (splatKinds.length != 2) {
            None
          } else {
            val dirs = splatKinds(0)
            val files = splatKinds(1)
            val splatFiles = files.map { k =>
              StorageKey[File](k(0), k(1))
            }
            val splatDirs = dirs.map { k =>
              StorageKey[Dir](k(0), k(1))
            }
            Some(splatFiles, splatDirs)
          }
        }
        (fileKeys, dirKeys) = allKeys
      } yield Dir(metadata, key, (dirKeys.map(_.right[StorageKey[File]]) ++ fileKeys.map(_.left[StorageKey[Dir]])).toList)
    }).map(_.flatten)
  }

  def enumerateDir(dir: Dir): StorageProgram[Option[List[StorageKey[FSEntry]]]] = {
    import StorageProgram._
    for {
      dir <- getDir(dir.key)
    } yield dir.map(_.childKeys)
  }

  def getFile(key: StorageKey[File]): StorageProgram[Option[File]] = {
    getRaw(key).map { raw =>
      ???
    }
  }

  def enumerate(path: Vector[String]): StorageProgram[Option[List[String]]] = {
    import StorageProgram._
    for {
      roots <- enumerateKey(fsroot)
      _ = roots.foldM[StorageProgram, Option[List[String]]](Some(List.empty[String]))((arr: Option[List[String]], opt: List[String]]) => ???)
      ne <- get(root)
    } yield ???
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

