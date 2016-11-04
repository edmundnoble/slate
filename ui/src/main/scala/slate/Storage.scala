package slate

import monix.eval.Task
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}
import cats.{Monad, Monoid, Traverse, ~>}
import cats.data._
import cats.implicits._
import org.atnos.eff._
import Eff._
import syntax.all._

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

  // finally tagless encoding isomorphism
  def storageToStorageActionTrans[F[_]](stor: Storage[F]): StorageAction ~> F = new (StorageAction ~> F) {
    override def apply[A](fa: StorageAction[A]): F[A] = fa.run(stor)
  }
  def storageFromStorageActionTrans[F[_]](nat: StorageAction ~> F): Storage[F] = new Storage[F] {
    override def apply(key: String): F[Option[String]] = nat(StorageAction.Get(key))
    override def update(key: String, data: String): F[Unit] = nat(StorageAction.Update(key, data))
    override def remove(key: String): F[Unit] = nat(StorageAction.Remove(key))
  }
}

// Finally tagless storage action functor (http://okmij.org/ftp/tagless-final/)
// Only using this because higher-kinded GADT refinement is broken,
// but dynamic dispatch is better than pattern matching on JS anyway.
sealed abstract class StorageAction[T] {
  def run[F[_]](storage: Storage[F]): F[T]
}

object StorageAction {

  final case class Get(key: String) extends StorageAction[Option[String]] {
    override def run[F[_]](storage: Storage[F]): F[Option[String]] = storage(key)
  }

  final case class Update(key: String, value: String) extends StorageAction[Unit] {
    override def run[F[_]](storage: Storage[F]): F[Unit] = storage.update(key, value)
  }

  final case class Remove(key: String) extends StorageAction[Unit] {
    override def run[F[_]](storage: Storage[F]): F[Unit] = storage.remove(key)
  }

  type _storageAction[R] = StorageAction <= R
  type _StorageAction[R] = StorageAction |= R

}

// DSL methods
object StorageProgram {

  import StorageAction._
  import Util._

  final def get[F: _StorageAction](key: String): Eff[F, Option[String]] =
    Get(key).send[F]

  final def update[F: _StorageAction](key: String, value: String): Eff[F, Unit] =
    Update(key, value).send[F]

  final def remove[F: _StorageAction](key: String): Eff[F, Unit] =
    Remove(key).send[F]

  final def getOrSet[F: _StorageAction](key: String, value: => String): Eff[F, String] = {
    for {
      cur <- get(key)
      result <- cur.fold(update[F](key, value).as(value))(_.pureEff[F])
    } yield result
  }

  def runProgram[S[_] : Monad, O, I, U, A](storage: Storage[S], program: Eff[I, A])
                                          (implicit ev: Member.Aux[StorageAction, I, U], ev2: Member.Aux[S, O, U]): Eff[O, A] = {
    interpret.transform[I, O, U, StorageAction, S, A](program, Storage.storageToStorageActionTrans(storage))
  }

  def printAction[A](act: StorageAction[A]): String = act match {
    case StorageAction.Update(k, v) => s"Update($k, $v)"
    case StorageAction.Get(k) => s"Get($k)"
    case StorageAction.Remove(k) => s"Remove($k"
  }

  // TODO: remove once added to eff-cats
  /**
    * Translate one effect of the stack into other effects in a larger stack
    */
  def translateInto[R, T[_], U, A](eff: Eff[R, A])(translate: interpret.Translate[T, U])(implicit m: MemberInOut[T, R], into: IntoPoly[R, U]): Eff[U, A] = {
    eff match {
      case Pure(a) => into(eff)
      case Impure(u, c) =>
        m.extract(u) match {
          case Some(tx) => translate(tx).flatMap(x => translateInto(c(x))(translate))
          case None => into(eff)
        }

      case ImpureAp(unions, c) =>
        val translated: Eff[U, List[Any]] = Eff.traverseA(unions.extract.effects)(tx => translate(tx))
        translated.flatMap(ts => translateInto(c(ts))(translate))
    }
  }

  def logProgram[I, O, L: Monoid, U, A](eff: Eff[I, A])(log: StorageAction ~> Const[L, ?])
                                       (implicit ev: Member.Aux[StorageAction, I, U],
                                        ev2: Member[StorageAction, O],
                                        ev3: MemberIn[Writer[L, ?], O],
                                        ev4: IntoPoly[I, O]): Eff[O, A] = {
    translateInto[I, StorageAction, O, A](eff)(new interpret.Translate[StorageAction, O] {
      override def apply[X](kv: StorageAction[X]): Eff[O, X] = {
        val logValue = log(kv).getConst
        for {
          _ <- writer.tell[O, L](logValue)
          r <- kv.send[O]
        } yield r
      }
    })
  }

  def logKeys[I, O, U, A](eff: Eff[I, A])
                         (implicit ev: Member.Aux[StorageAction, I, U],
                          ev2: Member[StorageAction, O],
                          ev3: MemberIn[Writer[Vector[String], ?], O],
                          ev4: IntoPoly[I, O]): Eff[O, A] =
    logProgram(eff)(new (StorageAction ~> Const[Vector[String], ?]) {
      override def apply[X](fa: StorageAction[X]): Const[Vector[String], X] = Const(fa match {
        case StorageAction.Get(k) => Vector.empty[String] :+ k
        case StorageAction.Update(k, _) => Vector.empty[String] :+ k
        case StorageAction.Remove(_) => Vector.empty[String]
      })
    })

  def logActions[I, O, U, A](eff: Eff[I, A])
                            (implicit ev: Member.Aux[StorageAction, I, U],
                             ev2: Member[StorageAction, O],
                             ev3: MemberIn[Writer[Vector[String], ?], O],
                             ev4: IntoPoly[I, O]): Eff[O, A] = {
    logProgram(eff)(new (StorageAction ~> Const[Vector[String], ?]) {
      override def apply[X](kv: StorageAction[X]): Const[Vector[String], X] = Const(kv match {
        case StorageAction.Get(k) => Vector.empty[String] :+ s"Get($k)"
        case StorageAction.Update(k, v) => Vector.empty[String] :+ s"Update($k, $v)"
        case StorageAction.Remove(k) => Vector.empty[String] :+ s"Remove($k)"
      })
    })
  }

  def retarget[R, U, A](eff: Eff[R, A])(prefix: String, delim: String)
                       (implicit ev: MemberInOut[StorageAction, R]
                       ): Eff[R, A] =
    interpret.interceptNat[R, U, StorageAction, A](eff)(new (StorageAction ~> StorageAction) {
      override def apply[X](kv: StorageAction[X]): StorageAction[X] =
        kv match {
          // TODO: clean up with access to SI-9760 fix
          case StorageAction.Get(k) => StorageAction.Get(prefix + delim + k).asInstanceOf[StorageAction[X]]
          case StorageAction.Update(k, v) => StorageAction.Update(prefix + delim + k, v).asInstanceOf[StorageAction[X]]
          case StorageAction.Remove(k) => StorageAction.Remove(prefix + delim + k).asInstanceOf[StorageAction[X]]
        }
    })

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
object PureStorage extends Storage[State[Map[String, String], ?]] {
  override def apply(key: String): State[Map[String, String], Option[String]] = State.get.map(_.get(key))

  override def update(key: String, data: String): State[Map[String, String], Unit] = State.modify(_ + (key -> data))

  override def remove(key: String): State[Map[String, String], Unit] = State.modify(_ - key)
}

