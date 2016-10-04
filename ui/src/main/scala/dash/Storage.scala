package dash

import monix.eval.Task
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}

import scalaz._
import scalaz.Scalaz._
import scalaz.syntax.functor._
import scalaz.syntax.bind._
import scalaz.syntax.applicative._

// Operations on a Storage with F[_] effects
// To abstract over storage that has different effects performed by its operations
// Examples of uses:
// localStorage, sessionStorage: use F = Task
// pure: Use F = State[Map[String, String], ?]]
abstract class Storage[F[_]] {
  def length: F[Int]

  def apply(key: String): F[Option[String]]

  def update(key: String, data: String): F[Unit]

  def clear(): F[Unit]

  def remove(key: String): F[Unit]

  def keyAtIndex(index: Int): F[Option[String]]
}

object Storage {

  import scalaz.Isomorphism._

  def storageFilter[F[_]](pred: String => Boolean): StorageProgram[Unit] = {
    for {
      len <- StorageProgram.length
      keys <- (0 to len).toList.traverse(StorageProgram.keyAtIndex(_)).map(_.map(_.get).filter(pred))
      _ <- keys.traverse(StorageProgram.remove)
    } yield ()
  }

  def toTrans[F[_]](storage: Storage[F]): StorageAction ~> F = new (StorageAction ~> F) {
    override def apply[A](fa: StorageAction[A]): F[A] = fa.run(storage)
  }

  def fromTrans[F[_]](trans: StorageAction ~> F): Storage[F] = new Storage[F] {
    override def length: F[Int] = trans(StorageAction.Length)
    override def apply(key: String): F[Option[String]] = trans(StorageAction.Get(key))
    override def update(key: String, data: String): F[Unit] = trans(StorageAction.Update(key, data))
    override def clear(): F[Unit] = trans(StorageAction.Clear)
    override def remove(key: String): F[Unit] = trans(StorageAction.Remove(key))
    override def keyAtIndex(index: Int): F[Option[String]] = trans(StorageAction.KeyAtIndex(index))
  }

  def log[F[_] : Applicative](storage: Storage[F]): Storage[LoggedStorage[F, ?]] =
    new Storage[WriterT[F, Vector[String], ?]] {
      override def length: WriterT[F, Vector[String], Int] =
        WriterT.put(storage.length)(Vector.empty[String])
      override def apply(key: String): WriterT[F, Vector[String], Option[String]] =
        WriterT.put(storage(key))(Vector.empty[String] :+ key)
      override def update(key: String, data: String): WriterT[F, Vector[String], Unit] =
        WriterT.put(storage.update(key, data))(Vector.empty[String] :+ key)
      override def clear(): WriterT[F, Vector[String], Unit] =
        WriterT.put(storage.clear())(Vector.empty[String])
      override def remove(key: String): WriterT[F, Vector[String], Unit] =
        WriterT.put(storage.remove(key))(Vector.empty[String])
      override def keyAtIndex(index: Int): WriterT[F, Vector[String], Option[String]] =
        WriterT.put(storage.keyAtIndex(index))(Vector.empty[String])
    }

  def emprefix[F[_] : Applicative : BindRec](storage: Storage[F]): Storage[Retargetable[F, ?]] = new Storage[ReaderT[F, String, ?]] {
    override def length: ReaderT[F, String, Int] =
      ReaderT.kleisli((_: String) => storage.length)
    override def apply(key: String): ReaderT[F, String, Option[String]] =
      ReaderT.kleisli((index: String) => storage.apply(index + " " + key))
    override def update(key: String, data: String): ReaderT[F, String, Unit] =
      ReaderT.kleisli((index: String) => storage.update(index + " " + key, data))
    override def clear(): ReaderT[F, String, Unit] =
      ReaderT.kleisli[F, String, Unit]((index: String) => StorageProgram.runProgram(storage, storageFilter[F](!_.startsWith(index + " "))))
    override def remove(key: String): ReaderT[F, String, Unit] =
      ReaderT.kleisli[F, String, Unit]((index: String) => storage.remove(index + " " + key))
    override def keyAtIndex(index: Int): ReaderT[F, String, Option[String]] =
      ReaderT.kleisli((_: String) => storage.keyAtIndex(index))
  }

  // finally tagless encoding isomorphism
  def storageActionNatIso[F[_]]: Storage[F] <=> (StorageAction ~> F) = new (Storage[F] <=> (StorageAction ~> F)) {
    override def to: (Storage[F]) => StorageAction ~> F = toTrans
    override def from: StorageAction ~> F => Storage[F] = fromTrans
  }
}

// Finally tagless storage action functor (http://okmij.org/ftp/tagless-final/)
// Only using this because higher-kinded GADT refinement is broken
sealed abstract class StorageAction[A] {
  def run[F[_]](storage: Storage[F]): F[A]
}

object StorageAction {

  type StorageActionF[A] = Coyoneda[StorageAction, A]

  case object Length extends StorageAction[Int] {
    override def run[F[_]](storage: Storage[F]): F[Int] = storage.length
  }

  final case class Get(key: String) extends StorageAction[Option[String]] {
    override def run[F[_]](storage: Storage[F]): F[Option[String]] = storage(key)
  }

  final case class KeyAtIndex(index: Int) extends StorageAction[Option[String]] {
    override def run[F[_]](storage: Storage[F]): F[Option[String]] = storage.keyAtIndex(index)
  }

  final case class Update(key: String, value: String) extends StorageAction[Unit] {
    override def run[F[_]](storage: Storage[F]): F[Unit] = storage.update(key, value)
  }

  final case class Remove(key: String) extends StorageAction[Unit] {
    override def run[F[_]](storage: Storage[F]): F[Unit] = storage.remove(key)
  }

  case object Clear extends StorageAction[Unit] {
    override def run[F[_]](storage: Storage[F]): F[Unit] = storage.clear()
  }

}

// DSL methods
object StorageProgram {

  import StorageAction._
  import Util._

  @inline final def length: StorageProgram[Int] =
    liftFC(Length)

  @inline final def get(key: String): StorageProgram[Option[String]] =
    liftFC(Get(key))

  @inline final def keyAtIndex(index: Int): StorageProgram[Option[String]] =
    liftFC(KeyAtIndex(index))

  @inline final def update(key: String, value: String): StorageProgram[Unit] =
    liftFC(Update(key, value))

  @inline final def remove(key: String): StorageProgram[Unit] =
    liftFC(Remove(key))

  @inline final def clear: StorageProgram[Unit] =
    liftFC(Clear)

  @inline def runProgram[F[_] : Applicative : BindRec, A](storage: Storage[F], program: StorageProgram[A]): F[A] =
    foldMapFCRec(program, Storage.toTrans(storage))

  @inline def sealProgram[F[_] : Applicative : BindRec, A](storage: Storage[F], index: String, program: StorageProgram[A]): F[A] = {
    implicit val functor: Functor[F] = implicitly[Applicative[F]]
    implicit val bind: Bind[F] = implicitly[BindRec[F]]
    bind.join[A](functor.map(StorageProgram.runProgram[Retargetable[LoggedStorage[F, ?], ?], A](
      Storage.emprefix(Storage.log(storage)),
      program
    ).run(index).run) { case (keys, out) =>
      functor.map[Unit, A](StorageProgram.runProgram[Retargetable[F, ?], Unit](
        Storage.emprefix(storage),
        Storage.storageFilter((str: String) => keys.contains(str) || !str.startsWith(index))).run(index)
      )(_ => out)
    })
  }

  @inline def runRetargetable[A]
  (index: String, program: Retargetable[StorageProgram, A]): StorageProgram[A] = program.run(index)

  @inline def runLoggedStorage[A]
  (program: LoggedStorage[StorageProgram, A]): StorageProgram[(Vector[String], A)] = program.run
}

// Implementation for dom.ext.Storage values
sealed class DomStorage(underlying: SStorage) extends Storage[Task] {

  import Task.eval

  override def length: Task[Int] = eval(underlying.length)

  override def apply(key: String): Task[Option[String]] = eval(underlying(key))

  override def update(key: String, data: String): Task[Unit] = eval(underlying.update(key, data))

  override def clear(): Task[Unit] = eval(underlying.clear())

  override def remove(key: String): Task[Unit] = eval(underlying.remove(key))

  override def keyAtIndex(index: Int): Task[Option[String]] = eval(underlying.key(index))
}

object DomStorage {

  case object Local extends DomStorage(LocalStorage)

  case object Session extends DomStorage(SessionStorage)

}

// Implementation for pure maps
object PureStorage extends Storage[State[String ==>> String, ?]] {

  import State._

  override def length: State[String ==>> String, Int] = get.map(_.size)

  override def apply(key: String): State[String ==>> String, Option[String]] = get.map(_.lookup(key))

  override def update(key: String, data: String): State[String ==>> String, Unit] = modify(_.insert(key, data))

  override def clear(): State[String ==>> String, Unit] = put(==>>.empty)

  override def remove(key: String): State[String ==>> String, Unit] = modify(_ - key)

  override def keyAtIndex(index: Int): State[String ==>> String, Option[String]] = get.map(_.elemAt(index).map(_._2))
}

