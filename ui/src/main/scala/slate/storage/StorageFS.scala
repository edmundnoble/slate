package slate
package storage

import cats.data.Xor
import cats.{Monad, RecursiveTailRecM}

import scalajs.js
import cats.implicits._
import slate.util.DelimitTransform

import scala.scalajs.js.Array
import scala.util.Try
import org.atnos.eff._, Eff._, syntax.all._

object StorageFS {
  final def fsroot: StorageKey[Dir] = StorageKey[Dir]("fsroot", "fsroot")

  // TODO: static safety for escaping these
  import slate.util._, DelimitTransform._

  object Delimiters {
    def interMetadataDelimiter = ":"
    def keyDelimiter = "\\"
    def metadataDelimiter = "/"
    def nodeKindDelimiter = ":"
    def interNodeDelimiter = ","
    def hashKeyDelimiter = ";"
  }

  object Dir {

    import Delimiters._

    def nodesStructure: DelimitTransform[Array[(String, String)]] =
      id.joinWithDelimiter(keyDelimiter, id)
        .thenDelimitBy(interNodeDelimiter)
    def structure: DelimitTransform[Dir] =
      Metadata.structure.joinWithDelimiter(
        metadataDelimiter,
        nodesStructure.joinWithDelimiter(nodeKindDelimiter, nodesStructure)
      ).imap[Dir]({ case (nm, (fileKeys, dirKeys)) =>
        val childKeys =
          fileKeys.map(t => StorageKey[File](t._1, t._2).left[StorageKey[Dir]]) ++ dirKeys.map(t => StorageKey[Dir](t._1, t._2).right[StorageKey[File]])
        Dir(nm, childKeys)
      }, { dir =>
        (dir.metadata, (dir.childFileKeys.map { k => (k.name, k.nonce) }, dir.childDirKeys.map { k => (k.name, k.nonce) }))
      })
  }
  object File {

    import Delimiters._

    def structure: DelimitTransform[File] =
      Metadata.structure.joinWithDelimiter(metadataDelimiter,
        id.joinWithDelimiter(hashKeyDelimiter,
          id.joinWithDelimiter(keyDelimiter, id)
        )
      ).imap(
        { case (nm, (str1, (str2, str3))) => File(nm, str1, StorageKey(str2, str3)) }, { file => (file.metadata, (file.dataHash, (file.dataKey.name, file.dataKey.nonce))) })
  }
  object Metadata {

    import Delimiters._

    def structure: DelimitTransform[NodeMetadata] =
      id.joinWithDelimiter(interMetadataDelimiter, id).imapX({
        case (lastUpdatedStr, lastAccessedStr) => (parseDate(lastUpdatedStr) |@| parseDate(lastAccessedStr)).map(NodeMetadata)
      }, {
        case NodeMetadata(lastUpdated, lastAccessed) => (lastUpdated.toISOString(), lastAccessed.toISOString())
      })
  }

  def parseDate(str: String): Option[js.Date] =
    Try(new js.Date(js.Date.parse(str))).toOption

  final case class StorageKey[F](name: String, nonce: String) {
    def render: String = name + Delimiters.keyDelimiter + nonce
  }

  final case class FileData(data: String)

  final case class NodeMetadata(lastUpdated: js.Date, lastAccessed: js.Date)
  final case class File(metadata: NodeMetadata, dataHash: String, dataKey: StorageKey[FileData])
  final case class Dir(metadata: NodeMetadata, childKeys: Array[FSKey]) {
    def childFileKeys: Array[StorageKey[File]] = childKeys.collect { case Xor.Left(fileKey) => fileKey }
    def childDirKeys: Array[StorageKey[Dir]] = childKeys.collect { case Xor.Right(dirKey) => dirKey }
  }
  type FSKey = StorageKey[File] Xor StorageKey[Dir]
  type FSEntry = File Xor Dir

  def getRaw(key: StorageKey[_]): StorageProgram[Option[String]] = {
    import StorageProgram._
    for {
      raw <- get(key.name + Delimiters.keyDelimiter + key.nonce)
    } yield raw
  }

  // TODO: error for Date.parse problem
  def readMetadata(lastUpdated: String, lastAccessed: String): Option[NodeMetadata] = {
    for {
      lastUpdatedParsed <- Try(new js.Date(js.Date.parse(lastUpdated))).toOption
      lastAccessedParsed <- Try(new js.Date(js.Date.parse(lastAccessed))).toOption
    } yield NodeMetadata(lastUpdatedParsed, lastAccessedParsed)
  }

  def getDir(key: StorageKey[Dir]): StorageProgram[Option[Dir]] = {
    for {
      raw <- getRaw(key)
    } yield raw.flatMap(Dir.structure.toInterpret)
  }

  def getDirKey(path: Vector[String]): StorageProgram[Option[StorageKey[Dir]]] =
    for {
      dirKey <- getDirKeyFromRoot(path, fsroot)
    } yield dirKey

  def getDirKeyFromRoot[F](path: Vector[String], rootKey: StorageKey[Dir]): StorageProgram[Option[StorageKey[Dir]]] = {
    path.foldM[StorageProgram, Option[StorageKey[Dir]]](Some(rootKey)) { (b, s) =>
      b.traverse(getDir).map(_.flatten).map(_.flatMap(_.childDirKeys.find(_.name == s)))
    }
  }

  def getFile(key: StorageKey[File]): StorageProgram[Option[File]] = {
    for {
      raw <- getRaw(key)
    } yield raw.flatMap(File.structure.toInterpret)
  }

  def getDirPath(path: Vector[String]): StorageProgram[Option[Dir]] = {
    for {
      dirKey <- getDirKey(path)
      dir <- dirKey.traverseM(getDir)
    } yield dir
  }

  def getFilePath[F[_]](path: Vector[String]): StorageProgram[Option[File]] = {
    for {
      dirKey <- getDirKey(path.init)
      dir <- dirKey.traverseM(getDir)
      fileKey = dir.flatMap(_.childFileKeys.find(_.name == path.last))
      file <- fileKey.traverseM(getFile)
    } yield file
  }

  def makeNonce(): String = ???

}

import StorageFS._

sealed class StorageFS[F[_] : Monad : RecursiveTailRecM](underlying: Storage[F], dirKey: StorageKey[Dir]) extends Storage[F] {

  import StorageFS._

  override def apply(key: String): F[Option[String]] = for {
    dir <- StorageProgram.runProgram(underlying, getDir(dirKey)).detach
    hash = dir.flatMap(_.childFileKeys.find(_.name == key))
    file <- hash.traverse[F, Option[File]](k => StorageProgram.runProgram(underlying, getFile(k)).detach)
    firstResult <- file.flatten.traverse[F, Option[String]](f => underlying(f.dataKey.render))
  } yield firstResult.flatten

  override def update(key: String, data: String): F[Unit] = for {
    dir <- StorageProgram.runProgram(underlying, getDir(dirKey)).detach
    hash = dir.map(_.childFileKeys.find(_.name == key))
    file <- hash.map { maybeHash =>
      maybeHash.map(_.pure[F]).getOrElse[F[StorageKey[File]]] {
        val storageKey = StorageKey[File](key, makeNonce())
        underlying.update(dirKey.render, Dir.structure.fromInterpret(
          dir.get.copy(childKeys = dir.get.childKeys :+ storageKey.left)
        )).as(storageKey)
      }
    }.traverse[F, Option[File]](_.flatMap(k => StorageProgram.runProgram(underlying, getFile(k)).detach))
    _ <- file.flatten.traverse[F, Unit](f => underlying.update(f.dataKey.render, data))
  } yield ()

  override def remove(key: String): F[Unit] = for {
    dir <- StorageProgram.runProgram(underlying, getDir(dirKey)).detach
    hash = dir.flatMap(_.childFileKeys.find(_.name == key))
    file <- hash.traverseM[F, File](k => StorageProgram.runProgram(underlying, getFile(k)).detach)
    _ <- file.traverse[F, Unit](f => underlying.remove(f.dataKey.render))
    _ <- if (file.isEmpty) ().pure[F]
    else underlying.update(dirKey.render, Dir.structure.fromInterpret(dir.get.copy(childKeys = dir.get.childKeys.filter(_.merge[StorageKey[_]].name != key))))
  } yield ()

}

