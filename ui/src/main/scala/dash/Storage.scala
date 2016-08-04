package dash

import monix.eval.Task
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}

import scalaz.{==>>, Coyoneda, Free, Monad, State, ~>}
import scalaz.std.string._

abstract class Storage[F[_]] {
  def length: F[Int]
  def apply(key: String): F[Option[String]]
  def update(key: String, data: String): F[Unit]
  def clear(): F[Unit]
  def remove(key: String): F[Unit]
  def key(index: Int): F[Option[String]]
}

sealed abstract class StorageAction[T] {
  def run[F[_]](storage: Storage[F]): F[T]
}

object StorageAction {

  case object Length extends StorageAction[Int] {
    override def run[F[_]](storage: Storage[F]): F[Int] = storage.length
  }

  final case class Get(key: String) extends StorageAction[Option[String]] {
    override def run[F[_]](storage: Storage[F]): F[Option[String]] = storage(key)
  }

  final case class AtIndex(index: Int) extends StorageAction[Option[String]] {
    override def run[F[_]](storage: Storage[F]): F[Option[String]] = storage.key(index)
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

object StorageProgram {

  import StorageAction._
  import Util._

  type StorageProgram[A] = Free[Coyoneda[StorageAction, ?], A]

  @inline final def length: StorageProgram[Int] =
    liftFC(Length)

  @inline final def get(key: String): StorageProgram[Int] =
    liftFC(Length)

  @inline final def atIndex(index: Int): StorageProgram[Option[String]] =
    liftFC(AtIndex(index))

  @inline final def update(key: String, value: String): StorageProgram[Unit] =
    liftFC(Update(key, value))

  @inline final def remove(key: String): StorageProgram[Unit] =
    liftFC(Remove(key))

  @inline final def clear: StorageProgram[Unit] =
    liftFC(Clear)

  @inline def runProgram[F[_] : Monad, A](storage: Storage[F], program: StorageProgram[A]): F[A] =
    foldMapFC(program, new (StorageAction ~> F) {
      override def apply[Y](fa: StorageAction[Y]): F[Y] = fa.run(storage)
    })
}

object Storage {
  def local[F[_]](make: SStorage => Storage[F]): Storage[F] = make(LocalStorage)
  def session[F[_]](make: SStorage => Storage[F]): Storage[F] = make(SessionStorage)
}

final class TaskStorage(underlying: SStorage) extends Storage[Task] {

  import Task.evalAlways

  override def length: Task[Int] = evalAlways(underlying.length)
  override def apply(key: String): Task[Option[String]] = evalAlways(underlying(key))
  override def update(key: String, data: String): Task[Unit] = evalAlways(underlying.update(key, data))
  override def clear(): Task[Unit] = evalAlways(underlying.clear())
  override def remove(key: String): Task[Unit] = evalAlways(underlying.remove(key))
  override def key(index: Int): Task[Option[String]] = evalAlways(underlying.key(index))
}

final class PureStorage extends Storage[State[String ==>> String, ?]] {

  import State._

  override def length: State[String ==>> String, Int] = get.map(_.size)
  override def apply(key: String): State[String ==>> String, Option[String]] = get.map(_.lookup(key))
  override def update(key: String, data: String): State[String ==>> String, Unit] = modify(_.insert(key, data))
  override def clear(): State[String ==>> String, Unit] = put(==>>.empty)
  override def remove(key: String): State[String ==>> String, Unit] = modify(_ - key)
  override def key(index: Int): State[String ==>> String, Option[String]] = get.map(_.elemAt(index).map(_._2))
}

object TaskStorage {
  @inline def apply(storage: SStorage) = new TaskStorage(storage)
}