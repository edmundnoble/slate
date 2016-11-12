package slate
package storage

import cats.data.{Reader, Xor}
import cats.{Monad, RecursiveTailRecM}

import scalajs.js
import cats.implicits._
import slate.util.DelimitTransform
import shapeless.tag._

import scala.scalajs.js.Array
import scala.util.Try
import org.atnos.eff._, Eff._, syntax.all._

object StorageFS {
  final def fsroot: StorageKey[Dir] = StorageKey[Dir]("fsroot", "fsroot")

  import StorageAction._storageAction

  /** idempotent */
  def initFS[R: _storageAction]: Eff[R, Unit] = {
    for {
      root <- getDir(fsroot)
      _ <- root.fold {
        StorageProgram.update(fsroot.render, Dir.structure.fromInterpret(Dir(Array(), Array())))
      } { _ => ().pureEff[R] }
    } yield ()
  }

  // TODO: static safety for escaping these
  import slate.util._, DelimitTransform._

  object Delimiters {
    def keyDelimiter = "\\"
    def nodeKindDelimiter = "^"
    def interNodeDelimiter = ","
    def hashKeyDelimiter = ";"
  }

  object Dir {

    import Delimiters._

    def nodesStructure: DelimitTransform[Array[(String, String)]] =
      id.joinWithDelimiter(keyDelimiter, id)
        .thenDelimitBy(interNodeDelimiter)
    def structure: DelimitTransform[Dir] =
      nodesStructure.joinWithDelimiter(nodeKindDelimiter, nodesStructure)
        .imap[Dir]({ case (fileKeys, dirKeys) =>
        Dir(fileKeys.map((StorageKey.apply[File] _).tupled), dirKeys.map((StorageKey.apply[Dir] _).tupled))
      }, { dir =>
        (dir.childFileKeys.map { k => (k.name, k.nonce) }, dir.childDirKeys.map { k => (k.name, k.nonce) })
      })
  }

  object File {

    import Delimiters._

    def structure: DelimitTransform[File] =
      id.joinWithDelimiter(hashKeyDelimiter,
        id.joinWithDelimiter(keyDelimiter, id)
      ).imap(
        { case (str1, (str2, str3)) => File(str1, StorageKey(str2, str3)) },
        file => (file.dataHash, (file.dataKey.name, file.dataKey.nonce))
      )
  }

  def parseDate(str: String): Option[js.Date] =
    Try(new js.Date(js.Date.parse(str))).toOption

  final case class StorageKey[F](name: String, nonce: String) {
    def render: String = name + Delimiters.keyDelimiter + nonce
  }

  final case class FileData(data: String)

  final case class File(dataHash: String, dataKey: StorageKey[FileData])
  final case class Dir(childFileKeys: Array[StorageKey[File]], childDirKeys: Array[StorageKey[Dir]])

  def getRaw[R: _storageAction](key: StorageKey[_]): Eff[R, Option[String]] = {
    import StorageProgram._
    for {
      raw <- get(key.name + Delimiters.keyDelimiter + key.nonce)
    } yield raw
  }

  def getDir[R: _storageAction](key: StorageKey[Dir]): Eff[R, Option[Dir]] = {
    for {
      raw <- getRaw(key)
    } yield raw.flatMap(Dir.structure.toInterpret)
  }

  def getDirKey[R: _storageAction](path: Vector[String]): Eff[R, Option[StorageKey[Dir]]] =
    for {
      dirKey <- getDirKeyFromRoot(path, fsroot)
    } yield dirKey

  def getDirKeyFromRoot[R: _storageAction](path: Vector[String], rootKey: StorageKey[Dir]): Eff[R, Option[StorageKey[Dir]]] = {
    path.foldM[Eff[R, ?], Option[StorageKey[Dir]]](Some(rootKey)) { (b, s) =>
      b.traverseA(getDir[R]).map(_.flatten).map(_.flatMap(_.childDirKeys.find(_.name == s)))
    }
  }

  def getFile[R: _storageAction](key: StorageKey[File]): Eff[R, Option[File]] = {
    for {
      raw <- getRaw(key)
    } yield raw.flatMap(File.structure.toInterpret)
  }

  def getDirPath[R: _storageAction](path: Vector[String]): Eff[R, Option[Dir]] = {
    for {
      dirKey <- getDirKey[R](path)
      dir <- dirKey.traverseA(getDir[R])
    } yield dir.flatten
  }

  def getFilePath[R: _storageAction](path: Vector[String]): Eff[R, Option[File]] = {
    for {
      dirKey <- getDirKey[R](path.init)
      dir <- dirKey.traverseA(getDir[R])
      fileKey = dir.flatten.flatMap(_.childFileKeys.find(_.name == path.last))
      file <- fileKey.traverseA(getFile[R])
    } yield file.flatten
  }

  def makeNonce(): String = "1"

  def getFileData[R: _storageAction](fileName: String, dirKey: StorageKey[Dir]): Eff[R, Option[String]] = for {
    dir <- getDir[R](dirKey)
    hash = dir.flatMap(_.childFileKeys.find(_.name == fileName))
    file <- hash.traverseA(getFile[R])
    firstResult <- file.flatten.traverseA(f => StorageProgram.get[R](f.dataKey.render))
  } yield firstResult.flatten

  def updateFileData[R: _storageAction](fileName: String, data: String, dirKey: StorageKey[Dir]): Eff[R, Unit] = for {
    dir <- getDir[R](dirKey)
    hash = dir.map(_.childFileKeys.find(_.name == fileName))
    _ <- hash.traverseA { maybeHash =>
      maybeHash.map(_ => ().pureEff[R]).getOrElse[Eff[R, Unit]] {
        val dataKey = StorageKey[FileData](fileName, makeNonce())
        val fileKey = StorageKey[File](fileName, makeNonce())
        val newFile = File(data.hashCode.toString, dataKey)
        StorageProgram.update(dataKey.render, data) >>
          StorageProgram.update(fileKey.render, File.structure.fromInterpret(newFile)) >>
          StorageProgram.update(dirKey.render, Dir.structure.fromInterpret(
            dir.get.copy(childFileKeys = dir.get.childFileKeys :+ fileKey)
          ))
      }
    }
  } yield ()

  def mkDir[R: _storageAction](fileName: String, dirKey: StorageKey[Dir]): Eff[R, Unit] = for {
    dir <- getDir[R](dirKey)
    hash = dir.map(_.childFileKeys.find(_.name == fileName))
    _ <- hash.traverseA { maybeHash =>
      maybeHash.map(_ => ().pureEff[R]).getOrElse[Eff[R, Unit]] {
        val subDirKey = StorageKey[Dir](fileName, makeNonce())
        val newDir = Dir(js.Array(), js.Array())
        StorageProgram.update(subDirKey.render, Dir.structure.fromInterpret(newDir)) >>
          StorageProgram.update(dirKey.render, Dir.structure.fromInterpret(
            dir.get.copy(childDirKeys = dir.get.childDirKeys :+ subDirKey)
          ))
      }
    }
  } yield ()


  def removeFile[R: _storageAction](fileName: String, dirKey: StorageKey[Dir]): Eff[R, Unit] = for {
    dir <- getDir[R](dirKey)
    hash = dir.flatMap(_.childFileKeys.find(_.name == fileName))
    file <- hash.traverseA(k => for {f <- getFile(k); _ <- StorageProgram.remove[R](k.render)} yield f)
    u <- file.flatten.traverseA[R, Unit](f => StorageProgram.remove[R](f.dataKey.render))
    _ <- u.traverseA(_ =>
      StorageProgram.update[R](dirKey.render,
        Dir.structure.fromInterpret(dir.get.copy(childFileKeys = dir.get.childFileKeys.filter(_.name != fileName)))
      )
    )
  } yield ()

}

import StorageFS._

final class StorageFS[F[_] : Monad : RecursiveTailRecM](underlying: Storage[F], nonceSource: () => String, dirKey: StorageKey[Dir]) extends Storage[F] {

  import StorageFS._

  override def apply(key: String): F[Option[String]] =
    StorageProgram.runProgram(underlying, getFileData(key, dirKey)).detach

  override def update(key: String, data: String): F[Unit] =
    StorageProgram.runProgram(underlying, updateFileData(key, data, dirKey)).detach

  override def remove(key: String): F[Unit] =
    StorageProgram.runProgram(underlying, removeFile(key, dirKey)).detach

}

