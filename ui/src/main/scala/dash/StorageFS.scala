package dash

import java.util.Date

import cats.data.Xor
import cats.free.FreeT
import cats.{Functor, Monad, RecursiveTailRecM}

import scalajs.js
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}
import cats.implicits._
import dash.StorageFS.Dir
import dash.util.{DelimitTransform, DelimitTransformSym}

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

  case class FileData(data: String)

  final case class NodeMetadata(lastUpdated: js.Date, lastAccessed: js.Date)
  case class File(metadata: NodeMetadata, key: StorageKey[File], dataHash: String, dataKey: StorageKey[FileData])
  case class Dir(metadata: NodeMetadata, key: StorageKey[Dir], childKeys: Array[FSKey])
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

  // dir format:
  // metadata (last updated and last accessed datetime) separated by interMetadataDelimiter
  // then metadataDelimiter
  // then file keys separated from folder keys by nodeKindDelimiter, where keys consist of
  // node names, keyDelimiter, node nonces
  // and keys are separated from each other by interNodeDelimiter
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

  def enumerate(path: Vector[String]): StorageProgram[Option[List[String]]] = {
    import StorageProgram._
    for {
      rootDir <- getDir(fsroot)
    //      _ = rootDir.traverse
//          _ = rootDir.foldM[StorageProgram, Option[List[String]]](rootDir)((arr: Option[List[String]], opt: List[String]]) => ???)
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

