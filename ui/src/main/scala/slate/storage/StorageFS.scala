package slate
package storage

import cats.Monoid
import cats.data.Writer
import cats.implicits._
import org.atnos.eff.Eff._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import slate.storage.StorageAction.{_StorageAction, _lsAction}

import scala.scalajs.js
import scala.scalajs.js.{Array, WrappedArray}
import scala.util.Try

object StorageFS {
  // TODO: static safety for escaping these
  import slate.util._
  import DelimitTransform._

  final case class Key[+F](name: String, nonce: String) {
    def pmap[B]: Key[B] = this.asInstanceOf[Key[B]]

    def render: String = Key.codec.from(this)
  }

  object Key {
    val codec: DelimitTransform[Key[_]] =
      (string | Delimiters.keyDelimiter | string)
        .imap { case (name, nonce) => Key[Any](name, nonce) } { k => (k.name, k.nonce) }
  }

  val fsroot: Key[Dir] = Key[Dir]("fsroot", "fsroot")

  import StorageAction._storageAction

  /** idempotent */
  def initFS[R: _storageAction](rootDir: Option[Dir]): Eff[R, Dir] = {
    for {
      newRoot <- rootDir.map(_.pureEff[R]).getOrElse {
        StorageProgram.update[R](fsroot.render, Dir.codec.from(Dir.empty)).as(Dir.empty)
      }
    } yield newRoot
  }

  import qq.util.Unsafe._
  import qq.Platform.Js.Unsafe._

  /** idempotent */
  def checkFS[R: _storageAction : _lsAction](rootDir: Option[Dir]): Eff[R, Dir] = {
    for {
      initializedRoot <- initFS[R](rootDir)
      allKnownKeys <- foldKeys(initializedRoot,
        v => Set(v.render), v => Set(v.render)
      )
      allKeys <- Eff.send[LsAction, R, List[String]](StorageAction.LsKeys)
      // TODO: check if this works :D
      //_ <- allKeys.traverseA(key => if (!allKnownKeys(key)) StorageProgram.remove(key) else ().pureEff)
      _ = println(allKnownKeys -- allKeys)
    } yield initializedRoot
  }

  object Delimiters {
    def keyDelimiter = "\\"

    def nodeKindDelimiter = "^"

    def interNodeDelimiter = ","
  }

  final case class Dir(childFileKeys: Array[Key[File]], childDirKeys: Array[Key[Dir]]) {
    def isEmpty: Boolean =
      childFileKeys.isEmpty && childDirKeys.isEmpty

    def addFileKey(key: Key[File]): Dir =
      copy(childFileKeys = childFileKeys :+ key)

    def addDirKey(key: Key[Dir]): Dir =
      copy(childDirKeys = childDirKeys :+ key)

    def findFileByName(fileName: String): Option[Key[File]] =
      childFileKeys.find(_.name == fileName)

    def findDirByName(dirName: String): Option[Key[Dir]] =
      childDirKeys.find(_.name == dirName)
  }

  object Dir {

    import Delimiters._

    val empty: Dir = Dir(js.Array(), js.Array())

    val nodesCodec: DelimitTransform[Array[Key[_]]] =
      Key.codec.splitBy(interNodeDelimiter)
    val codec: DelimitTransform[Dir] =
      (nodesCodec | nodeKindDelimiter | nodesCodec)
        .imap { case (fileKeys, dirKeys) =>
          Dir(fileKeys.map(_.pmap[File]), dirKeys.map(_.pmap[Dir]))
        } { dir =>
          (dir.childFileKeys.map(_.pmap[Any]), dir.childDirKeys.map(_.pmap[Any]))
        }

  }

  final case class File(data: String) extends AnyVal

  def parseDate(str: String): Option[js.Date] =
    Try(new js.Date(js.Date.parse(str))).toOption

  def getRaw[R: _StorageAction](key: Key[_]): Eff[R, Option[String]] =
    StorageProgram.get(key.name + Delimiters.keyDelimiter + key.nonce)

  def getDir[R: _StorageAction](key: Key[Dir]): Eff[R, Option[Dir]] = {
    for {
      raw <- getRaw[R](key)
    } yield raw.flatMap(Dir.codec.to)
  }

  def getDirKey[R: _storageAction](path: Vector[String]): Eff[R, Option[Key[Dir]]] =
    getDirKeyFromRoot(path, fsroot)

  def getDirKeyFromRoot[R: _storageAction](path: Vector[String], rootKey: Key[Dir]): Eff[R, Option[Key[Dir]]] = {
    path.foldM[Eff[R, ?], Option[Key[Dir]]](Some(rootKey)) { (b, s) =>
      b.traverseA(getDir[R]).map(_.flatten).map(_.flatMap(_.childDirKeys.find(_.name == s)))
    }
  }

  def getFile[R: _storageAction](key: Key[File]): Eff[R, Option[File]] = {
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

  def getFileInDir[R: _storageAction](fileName: String, dirKey: Key[Dir]): Eff[R, Option[String]] = for {
    dir <- getDir[R](dirKey)
    hash = dir.flatMap(_.findFileByName(fileName))
    file <- hash.flatTraverse[Eff[R, ?], File](getFile[R])
  } yield file.map(_.data)

  def updateFileInDir[R: _storageAction](fileName: String, nonceSource: () => String, data: String, dirKey: Key[Dir]): Eff[R, Option[Key[File]]] = for {
    maybeDir <- getDir[R](dirKey)
    maybeDirAndMaybeFileKey = maybeDir.fproduct(_.findFileByName(fileName))
    key <- maybeDirAndMaybeFileKey.traverseA {
      case (dir, maybeFileKey) =>
        maybeFileKey.map(fileKey =>
          StorageProgram.update(fileKey.render, data).as(fileKey)
        ).getOrElse[Eff[R, Key[File]]] {
          val fileKey = Key[File](fileName, nonceSource())
          for {
            _ <- StorageProgram.update(fileKey.render, data)
            _ <- StorageProgram.update(dirKey.render, Dir.codec.from(dir.addFileKey(fileKey)))
          } yield fileKey
        }
    }
  } yield key

  // returns None if outside does not exist.
  @inline
  def mkDir[R: _storageAction, B](dirName: String, nonceSource: () => String, outside: Key[Dir],
                                  alreadyPresent: (Dir, Key[Dir]) => B, dirMade: (Dir, Key[Dir]) => B): Eff[R, Option[B]] = for {
    maybeOutsideDir <- getDir[R](outside)
    maybeOutsideDirAndExistingKey = maybeOutsideDir.fproduct(_.findDirByName(dirName))
    out <- maybeOutsideDirAndExistingKey.traverseA {
      case (dir, maybeDirKey) =>
        maybeDirKey.fold[Eff[R, B]] {
          val subDirKey = Key[Dir](dirName, nonceSource())
          val newDir = Dir.empty
          (StorageProgram.update(subDirKey.render, Dir.codec.from(newDir)) >>
            StorageProgram.update(outside.render, Dir.codec.from(
              dir.addDirKey(subDirKey)
            ))).as(dirMade(Dir.empty, subDirKey))
        }(dirExistsAlready => alreadyPresent(dir, dirExistsAlready).pureEff[R])
    }
  } yield out

  def removeFile[R: _StorageAction, B](fileName: String, outsideKey: Key[Dir], wasNotPresent: B, wasPresent: B): Eff[R, B] = for {
    outsideDir <- getDir[R](outsideKey)
    insideFileKey = outsideDir.flatMap(_.childFileKeys.find(_.name == fileName))
    out <- insideFileKey.fold(wasNotPresent.pureEff[R])(k =>
      for {
        _ <- StorageProgram.remove[R](k.render)
        _ <- StorageProgram.update[R](outsideKey.render,
          Dir.codec.from(outsideDir.get.copy(childFileKeys = outsideDir.get.childFileKeys.filter(_.name != fileName)))
        )
      } yield wasPresent
    )
  } yield out

  def foldKeys[A, R: _storageAction](rootDir: Dir, fileKeyAction: Key[File] => A, dirKeyAction: Key[Dir] => A)
                                    (implicit A: Monoid[A]): Eff[R, A] =
    EffMonad[R].tailRecM[(WrappedArray[Dir], A), A]((WrappedArray(rootDir), A.empty)) { case (q, a) =>
      Eff.traverseA[R, WrappedArray, Dir, (WrappedArray[Dir], A) Either A](q) { dir =>
        val combinedFileKeys = A.combine(
          A.combineAll(new WrappedArray(dir.childFileKeys.map(fileKeyAction))),
          A.combineAll(new WrappedArray(dir.childDirKeys.map(dirKeyAction)))
        )
        val combineSoFar = A.combine(combinedFileKeys, a)
        if (dir.childDirKeys.isEmpty) {
          Eff.pure(Either.right[(WrappedArray[Dir], A), A](combineSoFar))
        } else {
          Eff.traverseA(new WrappedArray(dir.childDirKeys))(getDir[R])(builderTraverse[WrappedArray]).map(_.flatten).map(ks => Either.left[(WrappedArray[Dir], A), A]((ks, combineSoFar)))
        }
      }(builderTraverse[WrappedArray]).map { results =>
        val r = results.foldLeft((WrappedArray.empty[Dir], A.empty)) {
          case ((ar, d), bar) =>
            (ar ++ bar.fold(_._1, _ => WrappedArray.empty), A.combine(d, bar.fold(_._2, identity)))
        }
        if (r._1.isEmpty) Either.right(r._2)
        else Either.left(r)
      }
    }

  type StrVecWriter[A] = Writer[Vector[String], A]

  def runSealedStorageProgram[I, I2, U, U2, UN, A](prog: Eff[I, A], underlying: Storage[UN],
                                                   transform: Storage[UN] => Storage[U],
                                                   nonceSource: () => String,
                                                   storageRoot: StorageFS.Key[StorageFS.Dir])(
                                                    implicit ev: Member.Aux[StorageAction, I, U],
                                                    ev3: IntoPoly[UN, U2],
                                                    ev4: Member.Aux[StrVecWriter, U, U2],
                                                    ev5: Member.Aux[StrVecWriter, I, I2],
                                                    ev6: Member.Aux[StorageAction, I2, U2]
                                                  ): Eff[U2, A] = {
    val storageFS = transform(StorageFS(underlying, nonceSource, storageRoot))

    val loggedProgram: Eff[U, A] =
      StorageProgram.runProgram[I, U, A](LoggedKeyStorage(storageFS), prog)
    loggedProgram.runWriterFold(writer.MonoidFold[Vector[String]]).flatMap {
      case (v, usedKeys) =>
        StorageProgram.runProgram[I2, U2, A](underlying.into[U2](ev3), for {
          programDir <- StorageFS.getDir[I2](storageRoot)
          _ <- Eff.traverseA[I2, Option, Dir, List[Unit]](programDir) { dir =>
            val keysToRemove = dir.childFileKeys.map(_.name).toSet -- usedKeys
            keysToRemove.toList.traverseA(StorageFS.removeFile[I2, Unit](_, storageRoot, (), ()))
          }
        } yield v)(ev6)
    }
  }

}

import slate.storage.StorageFS._

final case class StorageFS[F](underlying: Storage[F], nonceSource: () => String, dirKey: Key[Dir]) extends Storage[F] {

  import StorageFS._

  override def apply(key: String): Eff[F, Option[String]] =
    StorageProgram.runProgram(underlying, getFileInDir[Fx.prepend[StorageAction, F]](key, dirKey))

  override def update(key: String, data: String): Eff[F, Unit] =
    StorageProgram.runProgram(underlying, updateFileInDir[Fx.prepend[StorageAction, F]](key, nonceSource, data, dirKey)).map(_ => ())

  override def remove(key: String): Eff[F, Unit] =
    StorageProgram.runProgram(underlying, removeFile[Fx.prepend[StorageAction, F], Unit](key, dirKey, (), ()))

}

