package dash

import cats.data.Xor
import cats.{Monad, RecursiveTailRecM}

import scalajs.js
import cats.implicits._
import dash.util.DelimitTransform

import scala.scalajs.js.Array
import scala.util.Try

object StorageFS {
  final def fsroot: StorageKey[Dir] = StorageKey[Dir]("fsroot", "fsroot")

  // TODO: static safety for escaping these
  object Delimiters {

    import dash.util._, DelimitTransform._

    object Dir {
      def nodesStructure: DelimitTransform[Array[(String, String)]] =
        id.joinWithDelimiter(keyDelimiter, id)
          .thenDelimitBy(interNodeDelimiter)
      def structure: DelimitTransform[((String, String), (Array[(String, String)], Array[(String, String)]))] =
        Metadata.structure.joinWithDelimiter(
          metadataDelimiter,
          nodesStructure.joinWithDelimiter(nodeKindDelimiter, nodesStructure))
      def nodeKindDelimiter = ":"
      def interNodeDelimiter = ","
    }
    object File {
      def structure: DelimitTransform[((String, String), (String, (String, String)))] =
        Metadata.structure.joinWithDelimiter(metadataDelimiter,
          id.joinWithDelimiter(hashKeyDelimiter,
            id.joinWithDelimiter(keyDelimiter, id)
          )
        )
      def hashKeyDelimiter = ";"
    }
    object Metadata {
      def structure: DelimitTransform[(String, String)] =
        id.joinWithDelimiter(interMetadataDelimiter, id)
    }
    def interMetadataDelimiter = ":"
    def keyDelimiter = "\\"
    def metadataDelimiter = "/"
  }

  final case class StorageKey[F](name: String, nonce: String) {
    def render: String = name + Delimiters.keyDelimiter + nonce
  }

  final case class FileData(data: String)

  final case class NodeMetadata(lastUpdated: js.Date, lastAccessed: js.Date)
  final case class File(metadata: NodeMetadata, key: StorageKey[File], dataHash: String, dataKey: StorageKey[FileData])
  final case class Dir(metadata: NodeMetadata, key: StorageKey[Dir], childKeys: Array[FSKey]) {
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
    getRaw(key).map(_.flatMap { raw =>
      val dirValues: Option[((String, String), (Array[(String, String)], Array[(String, String)]))] =
        DelimitTransform.interpret(Delimiters.Dir.structure)._1(raw)
      dirValues.flatMap {
        case ((lastUpdated, lastAccessed), (fileNodes, dirNodes)) =>
          for {
            metadata <- readMetadata(lastUpdated, lastAccessed)
          } yield
            Dir(metadata, key,
              fileNodes.map(t => StorageKey[File](t._1, t._2).left[StorageKey[Dir]]) ++ dirNodes.map(t => StorageKey[Dir](t._1, t._2).right[StorageKey[File]]))
      }
    })
  }

  def enumerateDir(dir: Dir): StorageProgram[Option[Array[FSKey]]] = {
    for {
      dir <- getDir(dir.key)
    } yield dir.map(_.childKeys)
  }

  def getDirKey(path: Vector[String]): StorageProgram[Option[StorageKey[Dir]]] =
    for {
      rootDir <- getDir(fsroot)
      dirKey <-
      if (path.isEmpty) rootDir.map(_.key).pure[StorageProgram]
      else for {
        lastDir <- findDirPath(path, rootDir)
      } yield lastDir.map(_.key)
    } yield dirKey

  private def findDirPath[F](path: Vector[String], rootDir: Option[Dir]): StorageProgram[Option[Dir]] = {
    path.foldM[StorageProgram, Option[Dir]](rootDir) { (b, s) =>
      b.flatMap(_.childDirKeys.find(_.name == s)).sequence[StorageProgram, Dir]
    }
  }

  def getFile(key: StorageKey[File]): StorageProgram[Option[File]] = {
    for {
      rawMaybe <- getRaw(key)
    } yield for {
      raw <- rawMaybe
      fileValues <- DelimitTransform.interpret(Delimiters.File.structure)._1(raw)
      ((lastUpdated, lastAccessed), (hash, (keyName, keyNonce))) = fileValues
      metadata <- readMetadata(lastUpdated, lastAccessed)
    } yield File(metadata, key, hash, StorageKey[FileData](keyName, keyNonce))
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

}

sealed class StorageFS[F[_] : Monad : RecursiveTailRecM](underlying: Storage[F], dir: StorageFS.Dir) extends Storage[F] {

  import StorageFS._

  override def apply(key: String): F[Option[String]] = for {
    hash <- dir.childFileKeys.find(_.name == key).pure[F]
    file <- hash.traverse[F, Option[File]](k => StorageProgram.runProgram(underlying, getFile(k)))
    firstResult <- file.flatten.traverse[F, Option[String]](f => underlying(f.dataKey.render))
  } yield firstResult.flatten

  override def update(key: String, data: String): F[Unit] = for {
    hash <- dir.childFileKeys.find(_.name == key).pure[F]
    file <- hash.traverse[F, Option[File]](k => StorageProgram.runProgram(underlying, getFile(k)))
    _ <- file.flatten.traverse[F, Unit](f => underlying.update(f.dataKey.render, data))
  } yield ()

  override def remove(key: String): F[Unit] = for {
    hash <- dir.childFileKeys.find(_.name == key).pure[F]
    file <- hash.traverseM[F, File](k => StorageProgram.runProgram(underlying, getFile(k)))
    _ <- file.traverse[F, Unit](f => underlying.remove(f.dataKey.render))
    _ <- underlying.update(dir.key, ???)
  } yield ()

}

