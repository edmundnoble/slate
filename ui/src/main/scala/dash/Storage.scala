package dash

import monix.eval.Task
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}

import scalaz.std.string._
import scalaz.{==>>, Applicative, BindRec, Coyoneda, State, WriterT, ~>}

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

// Finally tagless storage action functor (http://okmij.org/ftp/tagless-final/)
// Only using this because higher-kinded GADT refinement is broken
sealed abstract class StorageAction[T] {
  def run[F[_]](storage: Storage[F]): F[T]
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
    foldMapFCRec(program, new (StorageAction ~> F) {
      override def apply[Y](fa: StorageAction[Y]): F[Y] = fa.run(storage)
    })

  @inline def logProgram[F[_] : Applicative : BindRec](storage: Storage[F]): StorageAction ~> WriterT[F, Vector[String], ?] =
    new (StorageAction ~> WriterT[F, Vector[String], ?]) {
      override def apply[Y](fa: StorageAction[Y]): WriterT[F, Vector[String], Y] = {
        val keys = fa match {
          case StorageAction.Get(k) => Vector.empty[String] :+ k
          case StorageAction.Update(k, _) => Vector.empty[String] :+ k
          case _ => Vector.empty[String]
        }
        WriterT.put[F, Vector[String], Y](fa.run(storage))(keys)(implicitly[Applicative[F]])
      }
    }

  @inline def runRetargetableProgram[F[_] : Applicative : BindRec, A](storage: Storage[F], index: String, program: Retargetable[StorageProgram, A]): F[A] =
    foldMapFCRec(program.run(index), new (StorageAction ~> F) {
      override def apply[Y](fa: StorageAction[Y]): F[Y] = fa.run(storage)
    })
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

