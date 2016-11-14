package slate
package storage

import cats.data._
import cats.implicits._
import cats.{Applicative, Functor, Monad, Monoid, ~>}
import monix.eval.Task
import org.atnos.eff.Eff._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}

// Operations on a Storage with F[_] effects
// To abstract over text key-value storage that has different effects performed by its operations
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

  def runProgram[S[_] : Monad, O, I, U, A](storage: Storage[S], program: Eff[I, A])
                                          (implicit ev: Member.Aux[StorageAction, I, U], ev2: Member.Aux[S, O, U]): Eff[O, A] = {
    interpret.transform(program, Storage.storageToStorageActionTrans(storage))
  }

  def printAction: Storage[λ[A => String]] = new Storage[λ[A => String]] {
    override def apply(key: String): String = s"get($key)"
    override def update(key: String, data: String): String = s"update($key, $data)"
    override def remove(key: String): String = s"remove($key)"
  }

  // TODO: remove once added to eff-cats
  /**
    * Translate one effect of the stack into other effects in a larger stack
    */
  def translateInto[R, T[_], U, A](eff: Eff[R, A])(translate: interpret.Translate[T, U])
                                  (implicit m: MemberInOut[T, R], into: IntoPoly[R, U]): Eff[U, A] = {
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

  def logProgram[I, O, L: Monoid, U, A](eff: Eff[I, A])(log: StorageAction ~> Lambda[P => L])
                                       (implicit ev: Member.Aux[StorageAction, I, U],
                                        ev2: Member[StorageAction, O],
                                        ev3: MemberIn[Writer[L, ?], O],
                                        ev4: IntoPoly[I, O]): Eff[O, A] = {
    translateInto[I, StorageAction, O, A](eff)(new interpret.Translate[StorageAction, O] {
      override def apply[X](kv: StorageAction[X]): Eff[O, X] = {
        writer.tell[O, L](log(kv)) *> kv.send[O]
      }
    })
  }

  def logKeys[I, O, U, A](eff: Eff[I, A])
                         (implicit ev: Member.Aux[StorageAction, I, U],
                          ev2: Member[StorageAction, O],
                          ev3: MemberIn[Writer[Vector[String], ?], O],
                          ev4: IntoPoly[I, O]): Eff[O, A] =
    logProgram(eff)(new (StorageAction ~> λ[P => Vector[String]]) {
      override def apply[X](fa: StorageAction[X]): Vector[String] = fa match {
        case StorageAction.Get(k) => Vector.empty[String] :+ k
        case StorageAction.Update(k, _) => Vector.empty[String] :+ k
        case StorageAction.Remove(_) => Vector.empty[String]
      }
    })

  def logActions[I, O, U, A](eff: Eff[I, A])
                            (implicit ev: Member.Aux[StorageAction, I, U],
                             ev2: Member[StorageAction, O],
                             ev3: MemberIn[Writer[Vector[String], ?], O],
                             ev4: IntoPoly[I, O]): Eff[O, A] = {
    logProgram(eff)(new (StorageAction ~> λ[P => Vector[String]]) {
      override def apply[X](kv: StorageAction[X]): Vector[String] = kv match {
        case StorageAction.Get(k) => Vector.empty[String] :+ s"Get($k)"
        case StorageAction.Update(k, v) => Vector.empty[String] :+ s"Update($k, $v)"
        case StorageAction.Remove(k) => Vector.empty[String] :+ s"Remove($k)"
      }
    })
  }

  // specialize a storageProgram to work on a particular monad
  def toFixed[F[_] : Monad]: StorageProgram ~> FixedStorageAction[F, ?] = new (StorageProgram ~> FixedStorageAction[F, ?]) {
    override def apply[A](fa: StorageProgram[A]): FixedStorageAction[F, A] =
      (storage: Storage[F]) => StorageProgram.runProgram(storage, fa).detach
  }

}

// a StorageAction which only works with a single functor
sealed trait FixedStorageAction[F[_], A] {
  self =>

  def run(storage: Storage[F]): F[A]

  final def hflatMap[B](f: A => F[B])(implicit F: Monad[F]): FixedStorageAction[F, B] =
    (storage: Storage[F]) => F.flatMap(self.run(storage))(f)

  final def flatMap[B](f: A => FixedStorageAction[F, B])(implicit F: Monad[F]): FixedStorageAction[F, B] =
    (storage: Storage[F]) => F.flatMap(self.run(storage))(f(_).run(storage))

  final def map[B](f: A => B)(implicit F: Functor[F]): FixedStorageAction[F, B] =
    (storage: Storage[F]) => F.map(self.run(storage))(f)
}

object FixedStorageAction {

  def pure[F[_] : Applicative, A](value: A): FixedStorageAction[F, A] = (_: Storage[F]) => value.pure[F]

  def hpure[F[_], A](value: F[A]): FixedStorageAction[F, A] = (_: Storage[F]) => value

  def get[F[_]](key: String): FixedStorageAction[F, Option[String]] =
    (storage: Storage[F]) => storage(key)

  def update[F[_]](key: String, value: String): FixedStorageAction[F, Unit] =
    (storage: Storage[F]) => storage.update(key, value)

  def remove[F[_]](key: String): FixedStorageAction[F, Unit] =
    (storage: Storage[F]) => storage.remove(key)

  implicit def fixedStorageActionMonad[F[_]](implicit F: Monad[F]): Monad[FixedStorageAction[F, ?]] = new Monad[FixedStorageAction[F, ?]] {
    override def pure[A](x: A): FixedStorageAction[F, A] = FixedStorageAction.pure(x)
    override def flatMap[A, B](fa: FixedStorageAction[F, A])(f: (A) => FixedStorageAction[F, B]): FixedStorageAction[F, B] = fa.flatMap(f)
    override def map[A, B](fa: FixedStorageAction[F, A])(f: (A) => B): FixedStorageAction[F, B] = fa.map(f)
    override def tailRecM[A, B](a: A)(f: (A) => FixedStorageAction[F, Either[A, B]]): FixedStorageAction[F, B] = defaultTailRecM(a)(f)
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
object PureStorage extends Storage[State[Map[String, String], ?]] {
  override def apply(key: String): State[Map[String, String], Option[String]] =
    State.get.map(_.get(key))

  override def update(key: String, data: String): State[Map[String, String], Unit] =
    State.modify(_ + (key -> data))

  override def remove(key: String): State[Map[String, String], Unit] =
    State.modify(_ - key)
}

