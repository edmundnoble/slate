package slate
package storage

import cats.data._
import cats.implicits._
import org.atnos.eff.Eff._
import org.atnos.eff.addon.monix._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}
import slate.storage.StorageAction.LsKeys

// Operations on a Storage with F effects
// To abstract over text key-value storage that has different effects performed by its operations
// Examples of uses:
// localStorage, sessionStorage: use F = Task
// pure: Use F = State[Map[String, String], ?]]
trait Storage[F] {
  self =>
  def apply(key: String): Eff[F, Option[String]]

  def update(key: String, data: String): Eff[F, Unit]

  def remove(key: String): Eff[F, Unit]

  def into[G](implicit ev: IntoPoly[F, G]): Storage[G] = new Storage[G] {
    override def apply(key: String): Eff[G, Option[String]] =
      self(key).into[G]

    override def update(key: String, data: String): Eff[G, Unit] =
      self.update(key, data).into[G]

    override def remove(key: String): Eff[G, Unit] =
      self.remove(key).into[G]
  }
}

trait LsStorage[F] {
  self =>
  def lsKeys: Eff[F, List[String]]

  def into[G](implicit ev: IntoPoly[F, G]): LsStorage[G] = new LsStorage[G] {
    override def lsKeys: Eff[G, List[String]] =
      self.lsKeys.into[G]
  }
}

object LsStorage {

  // finally tagless encoding isomorphism
  def taglessToTrans[F](stor: LsStorage[F]): Translate[LsAction, F] = new Translate[LsAction, F] {
    override def apply[A](fa: LsAction[A]): Eff[F, A] = fa.run(stor)
  }

  def taglessFromTrans[F](nat: Translate[LsAction, F]): LsStorage[F] = new LsStorage[F] {
    override def lsKeys: Eff[F, List[String]] = nat(LsKeys)
  }
}

object Storage {

  def taglessFromTrans[F](nat: Translate[StorageAction, F]): Storage[F] = new Storage[F] {
    override def apply(key: String): Eff[F, Option[String]] = nat(StorageAction.Get(key))

    override def update(key: String, data: String): Eff[F, Unit] = nat(StorageAction.Update(key, data))

    override def remove(key: String): Eff[F, Unit] = nat(StorageAction.Remove(key))
  }

  // finally tagless encoding isomorphism
  def taglessToTrans[F](stor: Storage[F]): Translate[StorageAction, F] = new Translate[StorageAction, F] {
    override def apply[A](fa: StorageAction[A]): Eff[F, A] = fa.run(stor)
  }

  def composeIntos[F, G, H](fst: IntoPoly[F, G], snd: IntoPoly[G, H]): IntoPoly[F, H] = new IntoPoly[F, H] {
    def apply[A](e: Eff[F, A]): Eff[H, A] = snd(fst(e))
  }

}

// Finally tagless storage action monad (http://okmij.org/ftp/tagless-final/)
abstract class StorageAction[T] {
  def run[F](storage: Storage[F]): Eff[F, T]
}

abstract class LsAction[T] {
  def run[F](storage: LsStorage[F]): Eff[F, T]
}

object StorageAction {

  final case class Get(key: String) extends StorageAction[Option[String]] {
    override def run[F](storage: Storage[F]): Eff[F, Option[String]] =
      storage(key)
  }

  final case class Update(key: String, value: String) extends StorageAction[Unit] {
    override def run[F](storage: Storage[F]): Eff[F, Unit] =
      storage.update(key, value)
  }

  final case class Remove(key: String) extends StorageAction[Unit] {
    override def run[F](storage: Storage[F]): Eff[F, Unit] =
      storage.remove(key)
  }

  final case object LsKeys extends LsAction[List[String]] {
    override def run[F](storage: LsStorage[F]): Eff[F, List[String]] =
      storage.lsKeys
  }

  type _storageAction[R] = StorageAction <= R
  type _StorageAction[R] = StorageAction |= R
  type _lsAction[R] = LsAction <= R
  type _LsAction[R] = LsAction |= R

}

// DSL methods
object StorageProgram {

  import StorageAction._

  def get[F: _StorageAction](key: String): Eff[F, Option[String]] =
    Get(key).send[F]

  def update[F: _StorageAction](key: String, value: String): Eff[F, Unit] =
    Update(key, value).send[F]

  def remove[F: _StorageAction](key: String): Eff[F, Unit] =
    Remove(key).send[F]

  def getOrSet[F: _StorageAction](key: String, value: => String): Eff[F, String] = {
    for {
      cur <- get(key)
      result <- cur.fold(update[F](key, value).as(value))(_.pureEff[F])
    } yield result
  }

  def runProgram[I, U, A](storage: Storage[U], program: Eff[I, A])
                            (implicit ev: Member.Aux[StorageAction, I, U]): Eff[U, A] = {
    interpret.translate[I, U, StorageAction, A](program)(Storage.taglessToTrans[U](storage))
  }

  def runLsProgram[I, U, A](storage: LsStorage[U], program: Eff[I, A])
                              (implicit ev: Member.Aux[LsAction, I, U]): Eff[U, A] = {
    interpret.translate[I, U, LsAction, A](program)(LsStorage.taglessToTrans[U](storage))
  }

  def runProgramWithLsProgram[I, U, U2, A](storage: Storage[U2], lsStorage: LsStorage[U], program: Eff[I, A])
                                             (implicit ev: Member.Aux[StorageAction, U, U2],
                                              ev2: Member.Aux[LsAction, I, U]): Eff[U2, A] = {
    runProgram[U, U2, A](storage, runLsProgram[I, U, A](lsStorage, program).into)
  }


}

final case class LoggedKeyStorage[F: Member[Writer[Vector[String], ?], ?]](underlying: Storage[F]) extends Storage[F] {
  override def apply(key: String): Eff[F, Option[String]] =
    writer.tell[F, Vector[String]](key +: Vector.empty) >> underlying(key)

  override def update(key: String, data: String): Eff[F, Unit] =
    writer.tell[F, Vector[String]](key +: Vector.empty) >> underlying.update(key, data)

  override def remove(key: String): Eff[F, Unit] =
    underlying.remove(key)
}

final case class LoggedActionStorage[F: Member[Writer[Vector[String], ?], ?]](underlying: Storage[F]) extends Storage[F] {
  override def apply(key: String): Eff[F, Option[String]] =
    writer.tell[F, Vector[String]](s"Get($key)" +: Vector.empty) >> underlying(key)

  override def update(key: String, data: String): Eff[F, Unit] =
    writer.tell[F, Vector[String]](s"Update($key, $data)" +: Vector.empty) >> underlying.update(key, data)

  override def remove(key: String): Eff[F, Unit] =
    writer.tell[F, Vector[String]](s"Remove($key)" +: Vector.empty) >> underlying.remove(key)
}

// Implementation for dom.ext.Storage values
final class DomStorage[F: task._Task](underlying: SStorage) extends Storage[F] {
  override def apply(key: String): Eff[F, Option[String]] =
    task.taskDelay(underlying(key))

  override def update(key: String, data: String): Eff[F, Unit] =
    task.taskDelay(underlying.update(key, data))

  override def remove(key: String): Eff[F, Unit] =
    task.taskDelay(underlying.remove(key))
}

final class DomLsStorage[F: task._Task](underlying: SStorage) extends LsStorage[F] {

  override def lsKeys: Eff[F, List[String]] =
    task.taskDelay(
      (0 to underlying.length)
        .flatMap[String, List[String]](underlying.key)(collection.breakOut)
    )

}

object DomStorage {

  def Local[F: task._Task]: DomStorage[F] = new DomStorage[F](LocalStorage)

  def Session[F: task._Task]: DomStorage[F] = new DomStorage[F](SessionStorage)

  def LocalLs[F: task._Task]: DomLsStorage[F] = new DomLsStorage[F](LocalStorage)

  def SessionLs[F: task._Task]: DomLsStorage[F] = new DomLsStorage[F](SessionStorage)

}

// Implementation for pure maps
object PureStorage {

  type StringMapState[A] = State[StringMap, A]

  type StringMap = Map[String, String]

  type MapStateS = Fx.fx1[StringMapState]

  type MapStateE[A] = Eff[MapStateS, A]

  val Storage = new Storage[MapStateS] {

    override def apply(key: String): MapStateE[Option[String]] =
      Eff.send[StringMapState, MapStateS, Option[String]](State.get[Map[String, String]].map(_.get(key)))

    override def update(key: String, data: String): MapStateE[Unit] =
      Eff.send[StringMapState, MapStateS, Unit](State.modify[Map[String, String]](_ + (key -> data)))

    override def remove(key: String): MapStateE[Unit] =
      Eff.send[StringMapState, MapStateS, Unit](State.modify[Map[String, String]](_ - key))

  }

  val LsStorage = new LsStorage[MapStateS] {

    override def lsKeys: MapStateE[List[String]] =
      Eff.send[StringMapState, MapStateS, List[String]](State.get[Map[String, String]].map(_.keysIterator.toList))

  }

}

