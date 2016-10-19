package dash

import monix.eval.Task
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}

import scalaz.std.string._
import scalaz.{==>>, Applicative, BindRec, Coyoneda, Free, Functor, ReaderT, State, WriterT, ~>}
import scalaz.syntax.applicative._

// Operations on a Storage with F[_] effects
// To abstract over storage that has different effects performed by its operations
// Examples of uses:
// localStorage, sessionStorage: use F = Task
// pure: Use F = State[Map[String, String], ?]]
abstract class Storage[F[_]] {
  def apply(key: String): F[Option[String]]

  def update(key: String, data: String): F[Unit]

  def remove(key: String): F[Unit]
}

object Storage {

  import scalaz.Isomorphism._

  // finally tagless encoding isomorphism
  def storageStorageActionNatIso[F[_]]: Storage[F] <=> (StorageAction ~> F) = new (Storage[F] <=> (StorageAction ~> F)) {
    override def to: (Storage[F]) => StorageAction ~> F = (stor: Storage[F]) => new (StorageAction ~> F) {
      override def apply[A](fa: StorageAction[A]): F[A] = fa.run(stor)
    }
    override def from: StorageAction ~> F => Storage[F] = (nat: StorageAction ~> F) => new Storage[F] {
      override def apply(key: String): F[Option[String]] = nat(StorageAction.Get(key))
      override def update(key: String, data: String): F[Unit] = nat(StorageAction.Update(key, data))
      override def remove(key: String): F[Unit] = nat(StorageAction.Remove(key))
    }
  }
}

// Finally tagless storage action functor (http://okmij.org/ftp/tagless-final/)
// Only using this because higher-kinded GADT refinement is broken
sealed abstract class StorageAction[T] {
  def run[F[_]](storage: Storage[F]): F[T]
}

object StorageAction {

  type StorageActionF[A] = Coyoneda[StorageAction, A]

  final case class Get(key: String) extends StorageAction[Option[String]] {
    override def run[F[_]](storage: Storage[F]): F[Option[String]] = storage(key)
  }

  final case class Update(key: String, value: String) extends StorageAction[Unit] {
    override def run[F[_]](storage: Storage[F]): F[Unit] = storage.update(key, value)
  }

  final case class Remove(key: String) extends StorageAction[Unit] {
    override def run[F[_]](storage: Storage[F]): F[Unit] = storage.remove(key)
  }

}

// DSL methods
object StorageProgram {

  import StorageAction._
  import Util._

  @inline final def get(key: String): StorageProgram[Option[String]] =
    liftFC(Get(key))

  @inline final def update(key: String, value: String): StorageProgram[Unit] =
    liftFC(Update(key, value))

  @inline final def remove(key: String): StorageProgram[Unit] =
    liftFC(Remove(key))

  @inline final def getOrSet(key: String, value: => String): StorageProgram[String] = {
    for {
      cur <- get(key)
      o <- cur.fold(update(key, value) >| value)(_.pure[StorageProgram])
    } yield o
  }

  @inline def runProgram[F[_] : Applicative : BindRec, A](storage: Storage[F], program: StorageProgram[A]): F[A] =
    foldMapFCRec(program, new (StorageAction ~> F) {
      override def apply[Y](fa: StorageAction[Y]): F[Y] = fa.run(storage)
    })

  @inline def logNt: StorageAction ~> WriterT[StorageActionF, Vector[String], ?] =
    new (StorageAction ~> WriterT[StorageActionF, Vector[String], ?]) {
      override def apply[Y](fa: StorageAction[Y]): WriterT[StorageActionF, Vector[String], Y] = {
        val keys = fa match {
          case StorageAction.Get(k) => Vector.empty[String] :+ k
          case StorageAction.Update(k, _) => Vector.empty[String] :+ k
          case StorageAction.Remove(_) => Vector.empty[String]
        }
        WriterT.put[StorageActionF, Vector[String], Y](Coyoneda.lift(fa))(keys)
      }
    }

  @inline def retargetNt(delim: String): StorageActionF ~> Retargetable[StorageActionF, ?] =
    new (StorageActionF ~> Retargetable[StorageActionF, ?]) {
      override def apply[Y](fa: StorageActionF[Y]): Retargetable[StorageActionF, Y] = {
        ReaderT.kleisli[StorageActionF, String, Y]((prefix: String) => Coyoneda[StorageAction, fa.I, Y](fa.fi.asInstanceOf[StorageAction[Y]] match {
          // TODO: clean up with access to SI-9760 fix
          case StorageAction.Get(k) => StorageAction.Get(prefix + delim + k).asInstanceOf[StorageAction[fa.I]]
          case StorageAction.Update(k, v) => StorageAction.Update(prefix + delim + k, v).asInstanceOf[StorageAction[fa.I]]
          case StorageAction.Remove(k) => StorageAction.Remove(prefix + delim + k).asInstanceOf[StorageAction[fa.I]]
        })(fa.k))
      }
    }

  final def retarget[A](program: StorageProgram[A], delim: String): Free[dash.Retargetable[Coyoneda[StorageAction, ?], ?], A] = program.mapSuspension(new (Coyoneda[StorageAction, ?] ~> Retargetable[Coyoneda[StorageAction, ?], ?]) {
    override def apply[Y](fa: Coyoneda[StorageAction, Y]): Retargetable[Coyoneda[StorageAction, ?], Y] = StorageProgram.retargetNt(delim).apply(fa)
  })

  @inline def runRetargetableProgram[F[_] : Applicative : BindRec, A](storage: Storage[F], index: String, program: Free[Retargetable[Coyoneda[StorageAction, ?], ?], A]): F[A] = {
    val ran = program.mapSuspension(new (Retargetable[Coyoneda[StorageAction, ?], ?] ~> Coyoneda[StorageAction, ?]) {
      override def apply[Y](fa: Retargetable[Coyoneda[dash.StorageAction, ?], Y]): Coyoneda[StorageAction, Y] =
        fa.run(index)
    })
    runProgram(storage, ran)
  }
}

// Implementation for dom.ext.Storage values
sealed class DomStorage(underlying: SStorage) extends Storage[Task] {
  override def apply(key: String): Task[Option[String]] = Task.eval(underlying(key))

  override def update(key: String, data: String): Task[Unit] = Task.eval(underlying.update(key, data))

  override def remove(key: String): Task[Unit] = Task.eval(underlying.remove(key))
}

object DomStorage {
  case object Local extends DomStorage(LocalStorage)

  case object Session extends DomStorage(SessionStorage)
}

// Implementation for pure maps
object PureStorage extends Storage[State[String ==>> String, ?]] {
  override def apply(key: String): State[String ==>> String, Option[String]] = State.get.map(_.lookup(key))

  override def update(key: String, data: String): State[String ==>> String, Unit] = State.modify(_.insert(key, data))

  override def remove(key: String): State[String ==>> String, Unit] = State.modify(_ - key)
}

