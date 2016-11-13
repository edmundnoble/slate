package slate
package storage

import cats.Monad

import scalajs.js
import cats.implicits._

import scala.scalajs.js.Array
import scala.util.Try
import org.atnos.eff._
import Eff._
import cats.data.Writer
import syntax.all._

object StorageFS {
  final case class StorageKey[F](name: String, nonce: String) {
    def render: String = name + Delimiters.keyDelimiter + nonce
  }

  val fsroot: StorageKey[Dir] = StorageKey[Dir]("fsroot", "fsroot")

  import StorageAction._storageAction

  /** idempotent */
  def initFS[R: _storageAction]: Eff[R, Dir] = {
    for {
      root <- getDir(fsroot)
      newRoot <- root.map(_.pureEff[R]).getOrElse {
        StorageProgram.update[R](fsroot.render, Dir.structure.fromInterpret(Dir.empty)).as(Dir.empty)
      }
    } yield newRoot
  }

  // TODO: static safety for escaping these
  import slate.util._, DelimitTransform._

  object Delimiters {
    def keyDelimiter = "\\"
    def nodeKindDelimiter = "^"
    def interNodeDelimiter = ","
    def hashKeyDelimiter = ";"
  }

  final case class Dir(childFileKeys: Array[StorageKey[File]], childDirKeys: Array[StorageKey[Dir]]) {
    def isEmpty: Boolean = childFileKeys.isEmpty && childDirKeys.isEmpty
  }

  object Dir {

    import Delimiters._

    val empty: Dir = Dir(js.Array(), js.Array())

    def nodesStructure: DelimitTransform[Array[(String, String)]] =
      (id | keyDelimiter | id).thenDelimitBy(interNodeDelimiter)
    def structure: DelimitTransform[Dir] =
      (nodesStructure | nodeKindDelimiter | nodesStructure)
        .imap[Dir]({ case (fileKeys, dirKeys) =>
        Dir(fileKeys.map((StorageKey.apply[File] _).tupled), dirKeys.map((StorageKey.apply[Dir] _).tupled))
      }, { dir =>
        (dir.childFileKeys.map { k => (k.name, k.nonce) }, dir.childDirKeys.map { k => (k.name, k.nonce) })
      })
  }

  final case class File(dataHash: String, dataKey: StorageKey[FileData])

  final case class FileData(data: String)

  object File {

    import Delimiters._

    def structure: DelimitTransform[File] =
      (id | hashKeyDelimiter | (id | keyDelimiter | id)).imap(
        { case (str1, (str2, str3)) => File(str1, StorageKey(str2, str3)) },
        file => (file.dataHash, (file.dataKey.name, file.dataKey.nonce))
      )
  }

  def parseDate(str: String): Option[js.Date] =
    Try(new js.Date(js.Date.parse(str))).toOption


  def getRaw[R: _storageAction](key: StorageKey[_]): Eff[R, Option[String]] =
    StorageProgram.get(key.name + Delimiters.keyDelimiter + key.nonce)

  def getDir[R: _storageAction](key: StorageKey[Dir]): Eff[R, Option[Dir]] = {
    for {
      raw <- getRaw(key)
    } yield raw.flatMap(Dir.structure.toInterpret)
  }

  def getDirKey[R: _storageAction](path: Vector[String]): Eff[R, Option[StorageKey[Dir]]] =
    getDirKeyFromRoot(path, fsroot)

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

  def getFileData[R: _storageAction](fileName: String, dirKey: StorageKey[Dir]): Eff[R, Option[String]] = for {
    dir <- getDir[R](dirKey)
    hash = dir.flatMap(_.childFileKeys.find(_.name == fileName))
    file <- hash.traverseA(getFile[R])
    firstResult <- file.flatten.traverseA(f => StorageProgram.get[R](f.dataKey.render))
  } yield firstResult.flatten

  def updateFileData[R: _storageAction](fileName: String, nonceSource: () => String, data: String, dirKey: StorageKey[Dir]): Eff[R, Option[StorageKey[File]]] = for {
    dir <- getDir[R](dirKey)
    hash = dir.map(_.childFileKeys.find(_.name == fileName))
    dataKey = StorageKey[FileData](fileName, nonceSource())
    newFile = File(data.hashCode.toString, dataKey)
    key <- hash.traverseA { maybeHash =>
      maybeHash.map(fileKey => (StorageProgram.update(fileKey.render, File.structure.fromInterpret(newFile)) >>
        StorageProgram.update(dataKey.render, data)).as(fileKey)).getOrElse[Eff[R, StorageKey[File]]] {
        val fileKey = StorageKey[File](fileName, nonceSource())
        (StorageProgram.update(dataKey.render, data) >>
          StorageProgram.update(fileKey.render, File.structure.fromInterpret(newFile)) >>
          StorageProgram.update(dirKey.render, Dir.structure.fromInterpret(
            dir.get.copy(childFileKeys = dir.get.childFileKeys :+ fileKey)
          ))).as(fileKey)
      }
    }
  } yield key

  sealed abstract class MkDirResult {
    def fold[A](f: StorageKey[Dir] => A): A
  }
  final case class AlreadyPresent(key: StorageKey[Dir]) extends MkDirResult {
    def fold[A](f: StorageKey[Dir] => A): A = f(key)
  }
  final case class DirMade(key: StorageKey[Dir]) extends MkDirResult {
    def fold[A](f: StorageKey[Dir] => A): A = f(key)
  }

  def mkDir[R: _storageAction](fileName: String, nonceSource: () => String, dirKey: StorageKey[Dir]): Eff[R, Option[MkDirResult]] = for {
    dir <- getDir[R](dirKey)
    hash = dir.map(_.childDirKeys.find(_.name == fileName))
    key <- hash.traverseA { maybeHash =>
      maybeHash.fold[Eff[R, MkDirResult]] {
        val subDirKey = StorageKey[Dir](fileName, nonceSource())
        val newDir = Dir(js.Array(), js.Array())
        (StorageProgram.update(subDirKey.render, Dir.structure.fromInterpret(newDir)) >>
          StorageProgram.update(dirKey.render, Dir.structure.fromInterpret(
            dir.get.copy(childDirKeys = dir.get.childDirKeys :+ subDirKey)
          ))).as(DirMade(subDirKey))
      }(dirExistsAlready => (AlreadyPresent(dirExistsAlready): MkDirResult).pureEff[R])
    }
  } yield key


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

  def runSealedStorageProgram[F[_] : Monad, A](prog: StorageProgram[A], underlying: Storage[F], nonceSource: () => String,
                                               storageRoot: StorageFS.StorageKey[StorageFS.Dir]): F[A] = {
    val storageFS = new StorageFS(underlying, nonceSource, storageRoot)
    type mem = Member.Aux[Writer[Vector[String], ?], Fx.fx2[StorageAction, Writer[Vector[String], ?]], Fx.fx1[StorageAction]]
    val loggedProgram: F[(A, Vector[String])] = StorageProgram.runProgram(storageFS,
      writer.runWriterFold[Fx.fx2[StorageAction, Writer[Vector[String], ?]],
        Fx.fx1[StorageAction],
        Vector[String],
        A,
        Vector[String]](
        StorageProgram.logKeys[Fx.fx1[StorageAction],
          Fx.fx2[StorageAction, Writer[Vector[String], ?]],
          NoFx,
          A](prog)
      )(writer.MonoidFold[Vector[String]])(implicitly[mem])).detach
    loggedProgram.flatMap { case (v, usedKeys) =>
      val removeExcessProgram =
        StorageProgram.runProgram(underlying, for {
          programDir <- StorageFS.getDir[Fx.fx1[StorageAction]](storageRoot)
          _ <- programDir.traverse[StorageProgram, List[Unit]] { dir =>
            val keysToRemove = dir.childFileKeys.map(_.name).toSet -- usedKeys
            keysToRemove.toList.traverseA(StorageFS.removeFile[Fx.fx1[StorageAction]](_, storageRoot))
          }
        } yield ()).detach
      removeExcessProgram.as(v)
    }
  }

}

import StorageFS._

final class StorageFS[F[_] : Monad](underlying: Storage[F], nonceSource: () => String, dirKey: StorageKey[Dir]) extends Storage[F] {

  import StorageFS._

  override def apply(key: String): F[Option[String]] =
    StorageProgram.runProgram(underlying, getFileData(key, dirKey)).detach

  override def update(key: String, data: String): F[Unit] =
    StorageProgram.runProgram(underlying, updateFileData(key, nonceSource, data, dirKey)).detach.as(())

  override def remove(key: String): F[Unit] =
    StorageProgram.runProgram(underlying, removeFile(key, dirKey)).detach

}

