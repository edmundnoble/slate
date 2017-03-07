package slate
package storage

import cats.{Applicative, Monad, Monoid, Traverse}
import cats.data.{Writer, WriterT}
import cats.implicits._

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

  type DirKey = Key[Dir]
  type FileKey = Key[File]

  val fsroot: Key[Dir] = Key[Dir]("fsroot", "fsroot")

  /** idempotent */
  def initFS[F[_] : Applicative](rootDir: Option[Dir])(implicit storage: Storage[F]): F[Dir] =
    rootDir.fold {
      storage.update(fsroot.render, Dir.codec.from(Dir.empty)).as(Dir.empty)
    }(_.pure[F])

  import qq.Platform.Js.Unsafe._

  /** idempotent */
  def checkFS[F[_] : Monad](rootDir: Option[Dir])(implicit storage: Storage[F], lsStorage: LsStorage[F]): F[Dir] = {
    for {
      initializedRoot <- initFS[F](rootDir)
      allFoundKeys <- foldKeys[Set[String], F](
        initializedRoot,
        v => Set(v.render), v => Set(v.render)
      )(Monad[F], storage, catsStdInstancesForSet.algebra[String])
      allKnownKeys = allFoundKeys + fsroot.render
      allKeys <- lsStorage.lsKeys
      // TODO: check if this works :D
      //_ <- allKeys.traverseA(key => if (!allKnownKeys(key)) StorageProgram.remove(key) else ().pureEff)
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

  def getRaw[F[_]](key: Key[_])(implicit storage: Storage[F]): F[Option[String]] =
    storage(key.name + Delimiters.keyDelimiter + key.nonce)

  def getDir[F[_] : Monad](key: Key[Dir])(implicit storage: Storage[F]): F[Option[Dir]] = {
    for {
      raw <- getRaw(key)
    } yield raw.flatMap(Dir.codec.to)
  }

  def getRoot[F[_]: Monad](implicit storage: Storage[F]): F[Option[Dir]] =
    getDir[F](fsroot)

  def getDirKey[F[_] : Monad](path: Vector[String])(implicit storage: Storage[F]): F[Option[Key[Dir]]] =
    getDirKeyFromRoot[F](path, fsroot)

  def getDirKeyFromRoot[F[_] : Monad](path: Vector[String], rootKey: Key[Dir])(implicit storage: Storage[F]): F[Option[Key[Dir]]] = {
    path.foldM[F, Option[Key[Dir]]](Some(rootKey)) { (b, s) =>
      b.traverse[F, Option[Dir]](getDir[F]).map(_.flatten).map(_.flatMap(_.childDirKeys.find(_.name == s)))
    }
  }

  def getFile[F[_] : Monad](key: Key[File])(implicit storage: Storage[F]): F[Option[File]] = {
    for {
      raw <- getRaw(key)
    } yield raw.map(File)
  }

  def getDirPath[F[_] : Monad](path: Vector[String])(implicit storage: Storage[F]): F[Option[Dir]] = {
    for {
      dirKey <- getDirKey[F](path)
      dir <- dirKey.traverse(getDir[F])
    } yield dir.flatten
  }

  def getFileInDir[F[_] : Monad](fileName: String, dirKey: Key[Dir])(implicit storage: Storage[F]): F[Option[String]] = for {
    dir <- getDir[F](dirKey)
    hash = dir.flatMap(_.findFileByName(fileName))
    file <- hash.flatTraverse[F, File](getFile[F])
  } yield file.map(_.data)

  def updateFileInDir[F[_] : Monad](fileName: String, nonceSource: () => String, data: String, dirKey: Key[Dir])(implicit storage: Storage[F]): F[Option[Key[File]]] = for {
    maybeDir <- getDir[F](dirKey)
    maybeDirAndMaybeFileKey = maybeDir.fproduct(_.findFileByName(fileName))
    key <- maybeDirAndMaybeFileKey.traverse {
      case (dir, maybeFileKey) =>
        maybeFileKey.map(fileKey =>
          storage.update(fileKey.render, data).as(fileKey)
        ).getOrElse[F[Key[File]]] {
          val fileKey = Key[File](fileName, nonceSource())
          for {
            _ <- storage.update(fileKey.render, data)
            _ <- storage.update(dirKey.render, Dir.codec.from(dir.addFileKey(fileKey)))
          } yield fileKey
        }
    }
  } yield key

  // returns None if outside does not exist.
  @inline
  def mkDir[F[_] : Monad, B](dirName: String, nonceSource: () => String, outside: Key[Dir],
                             alreadyPresent: (Dir, Key[Dir]) => B, dirMade: (Dir, Key[Dir]) => B)(implicit storage: Storage[F]): F[Option[B]] = for {
    maybeOutsideDir <- getDir[F](outside)
    maybeOutsideDirAndExistingKey = maybeOutsideDir.fproduct(_.findDirByName(dirName))
    out <- maybeOutsideDirAndExistingKey.traverse {
      case (dir, maybeDirKey) =>
        maybeDirKey.fold[F[B]] {
          val subDirKey = Key[Dir](dirName, nonceSource())
          val newDir = Dir.empty
          (storage.update(subDirKey.render, Dir.codec.from(newDir)) >>
            storage.update(outside.render, Dir.codec.from(dir.addDirKey(subDirKey)))
            ).as(dirMade(Dir.empty, subDirKey))
        }(dirExistsAlready => alreadyPresent(dir, dirExistsAlready).pure[F])
    }
  } yield out

  @inline
  def removeFile[F[_] : Monad, B](fileName: String, outsideKey: Key[Dir], wasNotPresent: => B, wasPresent: => B)(implicit storage: Storage[F]): F[Option[B]] = {
    for {
      outsideDir <- getDir[F](outsideKey)
      insideFileKey = outsideDir.map(_.childFileKeys.find(_.name == fileName))
      out <- insideFileKey.traverse(_.fold(wasNotPresent.pure[F])(k =>
        for {
          _ <- storage.remove(k.render)
          _ <- storage.update(outsideKey.render,
            Dir.codec.from(outsideDir.get.copy(childFileKeys = outsideDir.get.childFileKeys.filter(_.name != fileName)))
          )
        } yield wasPresent
      ))
    } yield out
  }

  implicit val travvy: Traverse[WrappedArray] =
    qq.util.Unsafe.builderTraverse[WrappedArray]

  @inline
  // this could be more efficient: fuse something
  def foldKeys[A, F[_] : Monad](rootDir: Dir, fileKeyAction: Key[File] => A, dirKeyAction: Key[Dir] => A)
                               (implicit storage: Storage[F], A: Monoid[A]): F[A] =
    Monad[F].tailRecM[(WrappedArray[Dir], A), A]((WrappedArray(rootDir), A.empty)) { case (q, a) =>
      q.traverse[F, (WrappedArray[Dir], A) Either A] { dir =>
        val combinedFileKeys = A.combine(
          A.combineAll(new WrappedArray(dir.childFileKeys.map(fileKeyAction))),
          A.combineAll(new WrappedArray(dir.childDirKeys.map(dirKeyAction)))
        )
        val combineSoFar = A.combine(combinedFileKeys, a)
        val newDirs = new WrappedArray(dir.childDirKeys).traverse(getDir[F])
        if (dir.childDirKeys.isEmpty) {
          Either.right[(WrappedArray[Dir], A), A](combineSoFar).pure[F]
        } else {
          newDirs.map(_.flatten).map(ks => Either.left[(WrappedArray[Dir], A), A]((ks, combineSoFar)))
        }
      }.map { results =>
        val r = results.foldLeft((WrappedArray.empty[Dir], A.empty)) {
          case ((ar, d), bar) =>
            (ar ++ bar.fold(_._1, _ => WrappedArray.empty), A.combine(d, bar.fold(_._2, identity)))
        }
        if (r._1.isEmpty) Either.right(r._2)
        else Either.left(r)
      }
    }

  type StrVecWriter[A] = Writer[Vector[String], A]

  def runSealedStorageProgram[F[_] : Monad, A](program: Alg[Storage, Monad, A], storage: Storage[F],
                                               transform: Storage[F] => Storage[F],
                                               nonceSource: () => String,
                                               storageRoot: StorageFS.DirKey): F[A] = {
    val storageFS: Storage[WriterT[F, Vector[String], ?]] =
      LoggedKeyStorage(transform(StorageFS(storage, nonceSource, storageRoot)))

    val loggedProgramResult = program.apply(storageFS)
    for {
      loggedResult <- loggedProgramResult.run
      (usedKeys, v) = loggedResult
      dir <- getDir[F](storageRoot)(Monad[F], storage)
      keysToRemove = dir.map(_.childFileKeys.map(_.name).toSet -- usedKeys)
      _ <- keysToRemove.traverse(_.toList.traverse(removeFile[F, Unit](_, storageRoot, (), ())(Monad[F], storage)))
    } yield v
  }

}

import slate.storage.StorageFS._

final case class StorageFS[F[_] : Monad](underlying: Storage[F], nonceSource: () => String, dirKey: Key[Dir]) extends Storage[F] {

  import StorageFS._

  override def apply(key: String): F[Option[String]] =
    getFileInDir[F](key, dirKey)(Monad[F], underlying)

  //    StorageProgram.runProgram(underlying, getFileInDir[Fx.prepend[StorageAction, F]](key, dirKey))

  override def update(key: String, data: String): F[Unit] =
    updateFileInDir[F](key, nonceSource, data, dirKey)(Monad[F], underlying).map(_ => ())

  override def remove(key: String): F[Unit] =
    removeFile[F, Unit](key, dirKey, (), ())(Monad[F], underlying).map(_ => ())

}

