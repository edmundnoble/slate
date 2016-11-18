package slate
package storage

import cats.Monad
import cats.data.Writer
import cats.implicits._
import org.atnos.eff.Eff._
import org.atnos.eff._
import org.atnos.eff.syntax.all._

import scala.scalajs.js
import scala.scalajs.js.Array
import scala.util.Try

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
        StorageProgram.update[R](fsroot.render, Dir.structure.from(Dir.empty)).as(Dir.empty)
      }
    } yield newRoot
  }

  // TODO: static safety for escaping these
  import slate.util._
  import DelimitTransform._

  object Delimiters {
    def keyDelimiter = "\\"
    def nodeKindDelimiter = "^"
    def interNodeDelimiter = ","
  }

  final case class Dir(childFileKeys: Array[StorageKey[File]], childDirKeys: Array[StorageKey[Dir]]) {
    def isEmpty: Boolean = childFileKeys.isEmpty && childDirKeys.isEmpty
  }

  object Dir {

    import Delimiters._

    val empty: Dir = Dir(js.Array(), js.Array())

    def nodesStructure: DelimitTransform[Array[(String, String)]] =
      (string | keyDelimiter | string).splitBy(interNodeDelimiter)
    val structure: DelimitTransform[Dir] =
      (nodesStructure | nodeKindDelimiter | nodesStructure)
        .imap { case (fileKeys, dirKeys) =>
          Dir(fileKeys.map((StorageKey.apply[File] _).tupled), dirKeys.map((StorageKey.apply[Dir] _).tupled))
        } { dir =>
          (dir.childFileKeys.map { k => (k.name, k.nonce) }, dir.childDirKeys.map { k => (k.name, k.nonce) })
        }

  }

  final case class File(data: String) extends AnyVal

  def parseDate(str: String): Option[js.Date] =
    Try(new js.Date(js.Date.parse(str))).toOption

  def getRaw[R: _storageAction](key: StorageKey[_]): Eff[R, Option[String]] =
    StorageProgram.get(key.name + Delimiters.keyDelimiter + key.nonce)

  def getDir[R: _storageAction](key: StorageKey[Dir]): Eff[R, Option[Dir]] = {
    for {
      raw <- getRaw(key)
    } yield raw.flatMap(Dir.structure.to)
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
    } yield raw.map(File)
  }

  def getDirPath[R: _storageAction](path: Vector[String]): Eff[R, Option[Dir]] = {
    for {
      dirKey <- getDirKey[R](path)
      dir <- dirKey.traverseA(getDir[R])
    } yield dir.flatten
  }

  def getFileInDir[R: _storageAction](fileName: String, dirKey: StorageKey[Dir]): Eff[R, Option[String]] = for {
    dir <- getDir[R](dirKey)
    hash = dir.flatMap(_.childFileKeys.find(_.name == fileName))
    file <- hash.flatTraverse[Eff[R, ?], File](getFile[R])
  } yield file.map(_.data)

  def updateFileInDir[R: _storageAction](fileName: String, nonceSource: () => String, data: String, dirKey: StorageKey[Dir]): Eff[R, Option[StorageKey[File]]] = for {
    maybeDir <- getDir[R](dirKey)
    hash = maybeDir.fproduct(_.childFileKeys.find(_.name == fileName))
    key <- hash.traverseA {
      case (dir, maybeHash) =>
        maybeHash.map(fileKey => StorageProgram.update(fileKey.render, data)
          .as(fileKey)).getOrElse[Eff[R, StorageKey[File]]] {
          val fileKey = StorageKey[File](fileName, nonceSource())
          val updateAction =
            StorageProgram.update(fileKey.render, data) >>
              StorageProgram.update(dirKey.render, Dir.structure.from(
                dir.copy(childFileKeys = dir.childFileKeys :+ fileKey)
              ))
          updateAction.as(fileKey)
        }
    }
  } yield key

  sealed trait MkDirResult extends Any {
    def fold[A](f: StorageKey[Dir] => A): A
  }
  final case class AlreadyPresent(key: StorageKey[Dir]) extends AnyVal with MkDirResult {
    def fold[A](f: StorageKey[Dir] => A): A = f(key)
  }
  final case class DirMade(key: StorageKey[Dir]) extends AnyVal with MkDirResult {
    def fold[A](f: StorageKey[Dir] => A): A = f(key)
  }

  def mkDir[R: _storageAction](fileName: String, nonceSource: () => String, dirKey: StorageKey[Dir]): Eff[R, Option[MkDirResult]] = for {
    dir <- getDir[R](dirKey)
    hash = dir.map(_.childDirKeys.find(_.name == fileName))
    key <- hash.traverseA { maybeHash =>
      maybeHash.fold[Eff[R, MkDirResult]] {
        val subDirKey = StorageKey[Dir](fileName, nonceSource())
        val newDir = Dir(js.Array(), js.Array())
        (StorageProgram.update(subDirKey.render, Dir.structure.from(newDir)) >>
          StorageProgram.update(dirKey.render, Dir.structure.from(
            dir.get.copy(childDirKeys = dir.get.childDirKeys :+ subDirKey)
          ))).as(DirMade(subDirKey))
      }(dirExistsAlready => (AlreadyPresent(dirExistsAlready): MkDirResult).pureEff[R])
    }
  } yield key


  def removeFile[R: _storageAction](fileName: String, dirKey: StorageKey[Dir]): Eff[R, Unit] = for {
    dir <- getDir[R](dirKey)
    hash = dir.flatMap(_.childFileKeys.find(_.name == fileName))
    _ <- hash.traverseA(k =>
      for {
        _ <- StorageProgram.remove[R](k.render)
        _ <- StorageProgram.update[R](dirKey.render,
          Dir.structure.from(dir.get.copy(childFileKeys = dir.get.childFileKeys.filter(_.name != fileName)))
        )
      } yield ()
    )
  } yield ()

  def runSealedStorageProgram[I, O, U, F[_] : Monad, A](prog: Eff[I, A], underlying: Storage[F],
                                                        nonceSource: () => String,
                                                        storageRoot: StorageFS.StorageKey[StorageFS.Dir])(
                                                         implicit ev: Member.Aux[StorageAction, I, U],
                                                         ev2: Member.Aux[F, O, U]
                                                       ): Eff[O, A] = {
    val storageFS = new StorageFS(underlying, nonceSource, storageRoot)
    val loggedProgram: Eff[O, (A, Vector[String])] =
      StorageProgram.runProgram[F, O, I, U, (A, Vector[String])](storageFS,
        StorageProgram.withLoggedKeys(prog)
      )
    loggedProgram.flatMap { case (v, usedKeys) =>
      val removeExcessProgram: Eff[O, Unit] =
        StorageProgram.runProgram(underlying, for {
          programDir <- StorageFS.getDir[I](storageRoot)
          _ <- programDir.traverseA[I, List[Unit]] { dir =>
            val keysToRemove = dir.childFileKeys.map(_.name).toSet -- usedKeys
            keysToRemove.toList.traverseA(StorageFS.removeFile[I](_, storageRoot))
          }
        } yield ())
      removeExcessProgram.as(v)
    }
  }

  def runSealedStorageProgramInto[I, U, F[_] : Monad, A](prog: Eff[I, A], underlying: Storage[F],
                                                         nonceSource: () => String,
                                                         storageRoot: StorageFS.StorageKey[StorageFS.Dir])(
                                                          implicit ev: Member.Aux[StorageAction, I, U],
                                                          ev2: Member[F, U]
                                                        ): Eff[U, A] = {
    val storageFS = new StorageFS(underlying, nonceSource, storageRoot)
    type mem = Member.Aux[Writer[Vector[String], ?], Fx.prepend[Writer[Vector[String], ?], I], I]
    val loggedProgram: Eff[U, (A, Vector[String])] =
      StorageProgram.runProgramInto[F, I, U, (A, Vector[String])](storageFS,
        writer.runWriterFold[Fx.prepend[Writer[Vector[String], ?], I], I, Vector[String], A, Vector[String]](
          StorageProgram.logKeys[I, Fx.prepend[Writer[Vector[String], ?], I], U, A](prog)
        )(writer.MonoidFold[Vector[String]])(implicitly[mem])
      )
    loggedProgram.flatMap { case (v, usedKeys) =>
      val removeExcessProgram: Eff[U, Unit] =
        StorageProgram.runProgramInto(underlying, for {
          programDir <- StorageFS.getDir[I](storageRoot)
          _ <- programDir.traverseA[I, List[Unit]] { dir =>
            val keysToRemove = dir.childFileKeys.map(_.name).toSet -- usedKeys
            keysToRemove.toList.traverseA(StorageFS.removeFile[I](_, storageRoot))
          }
        } yield ())
      removeExcessProgram.as(v)
    }
  }

}

import slate.storage.StorageFS._

final class StorageFS[F[_] : Monad](underlying: Storage[F], nonceSource: () => String, dirKey: StorageKey[Dir]) extends Storage[F] {

  import StorageFS._

  override def apply(key: String): F[Option[String]] =
    StorageProgram.runProgram(underlying, getFileInDir(key, dirKey)).detach

  override def update(key: String, data: String): F[Unit] =
    StorageProgram.runProgram(underlying, updateFileInDir(key, nonceSource, data, dirKey)).detach.as(())

  override def remove(key: String): F[Unit] =
    StorageProgram.runProgram(underlying, removeFile(key, dirKey)).detach

}

