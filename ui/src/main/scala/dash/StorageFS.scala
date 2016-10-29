package dash

import java.util.Date

import cats.data.Xor
import cats.free.FreeT
import cats.{Functor, Monad, RecursiveTailRecM}

import scalajs.js
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}
import cats.implicits._
import dash.StorageFS.Dir

object StorageFS {
  final def fsroot: StorageKey[Dir] = StorageKey[Dir]("fsroot", "fsroot")

  // TODO: static safety for escaping these
  object Delimiters {
    object Dir {
      final val nodeKindDelimiter = ":"
      final val interNodeDelimiter = ","
    }
    object File {
      final val hashKeyDelimiter = ";"
    }
    final val interMetadataDelimiter = ":"
    final val keyDelimiter = "\\"
    final val metadataDelimiter = " "
  }

  final case class StorageKey[F](name: String, nonce: String) {
    def render: String = name + Delimiters.keyDelimiter + nonce
  }

  case class FileData(data: String)

  final case class NodeMetadata(lastUpdated: js.Date, lastAccessed: js.Date)
  case class File(metadata: NodeMetadata, key: StorageKey[File], dataHash: String, dataKey: StorageKey[FileData])
  case class Dir(metadata: NodeMetadata, key: StorageKey[Dir], childKeys: List[FSKey])
  type FSKey = StorageKey[File] Xor StorageKey[Dir]
  type FSEntry = File Xor Dir

  def getRaw(key: StorageKey[_]): StorageProgram[Option[String]] = {
    import StorageProgram._
    for {
      raw <- get(key.name + Delimiters.keyDelimiter + key.nonce)
    } yield raw
  }

  // TODO: error for Date.parse problem
  def readMetadata(metadataStr: String): Option[NodeMetadata] = {
    val splat = metadataStr.split(Delimiters.interMetadataDelimiter)
    for {
      dates <- for {
        safeSplat <- Some(splat) if splat.length == 2
        datesParsed = safeSplat.map(s => new js.Date(js.Date.parse(s)))
      } yield (datesParsed(0), datesParsed(1))
      (lastUpdated, lastAccessed) = dates
    } yield NodeMetadata(lastUpdated, lastAccessed)
  }

  // dir format:
  // metadata (last updated and last accessed datetime) separated by interMetadataDelimiter
  // then metadataDelimiter
  // then file keys separated from folder keys by nodeKindDelimiter, where keys consist of
  // node names, keyDelimiter, node nonces
  // and keys are separated from each other by interNodeDelimiter
  def getDir(key: StorageKey[Dir]): StorageProgram[Option[Dir]] = {
    getRaw(key).map(_.flatMap { raw =>
      val metadataIndex = raw.indexOf(Delimiters.metadataDelimiter)
      val metadataStr = raw.substring(0, metadataIndex)
      val dataStr = raw.substring(metadataIndex)
      for {
        metadata <- readMetadata(metadataStr)
        splatKinds = dataStr.split(Delimiters.Dir.nodeKindDelimiter).map(
          _.split(Delimiters.Dir.interNodeDelimiter).map(_.split(Delimiters.keyDelimiter))
        )
        if splatKinds.length == 2
        (fileKeys, dirKeys) = {
          val dirs = splatKinds(0)
          val files = splatKinds(1)
          val splatFiles = files.map { k =>
            StorageKey[File](k(0), k(1))
          }
          val splatDirs = dirs.map { k =>
            StorageKey[Dir](k(0), k(1))
          }
          (splatFiles, splatDirs)
        }
      } yield Dir(metadata, key, (dirKeys.map(_.right[StorageKey[File]]) ++ fileKeys.map(_.left[StorageKey[Dir]])).toList)
    })
  }

  def enumerateDir(dir: Dir): StorageProgram[Option[List[FSKey]]] = {
    for {
      dir <- getDir(dir.key)
    } yield dir.map(_.childKeys)
  }

  def getFile(key: StorageKey[File]): StorageProgram[Option[File]] = {
    (for {
      rawMaybe <- getRaw(key)
    } yield rawMaybe.map { raw =>
      val metadataIndex = raw.indexOf(Delimiters.metadataDelimiter)
      val metadataStr = raw.substring(0, metadataIndex)
      val dataStr = raw.substring(metadataIndex)
      for {
        metadata <- readMetadata(metadataStr)
        hashAndKey <- Some(dataStr.split(Delimiters.File.hashKeyDelimiter))
        if hashAndKey.length == 2
        (hash, dataKey) = (hashAndKey(0), hashAndKey(1))
        keySplit <- Some(hash.split(Delimiters.keyDelimiter))
        if keySplit.length == 2
      } yield File(metadata, key, hash, StorageKey[FileData](keySplit(0), keySplit(1)))
    }).map(_.flatten)
  }

  def enumerate(path: Vector[String]): StorageProgram[Option[List[String]]] = {
    import StorageProgram._
    for {
      rootDir <- getDir(fsroot)
//      _ = rootDir.traverse
    //      _ = roots.foldM[StorageProgram, Option[List[String]]](Some(List.empty[String]))((arr: Option[List[String]], opt: List[String]]) => ???)
    //      ne <- get(root)
    } yield ???
  }

  def getHash[F[_]](underlying: Storage[F], path: Vector[String]): F[String] = {
    ???
  }

  def allFolders[F[_]](underlying: Storage[F], root: String): F[List[String]] = {
    ???
  }
}

sealed class StorageFS[F[_] : Monad : RecursiveTailRecM](underlying: Storage[F], dir: Dir) extends Storage[F] {

  override def apply(key: String): F[Option[String]] = for {
    hash <- dir.childKeys.collect { case Xor.Left(fileKey) => fileKey }.find(_.name == key).pure[F]
    file <- hash.traverse[F, Option[StorageFS.File]](k => StorageProgram.runProgram(underlying, StorageFS.getFile(k)))
    firstResult <- file.flatten.traverse[F, Option[String]](f => underlying(f.dataKey.render))
  } yield firstResult.flatten

  override def update(key: String, data: String): F[Unit] = ???

  override def remove(key: String): F[Unit] = ???
}

