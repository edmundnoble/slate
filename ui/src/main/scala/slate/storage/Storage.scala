package slate
package storage

import cats.data._
import cats.implicits._
import cats.{Applicative, Monad}
import monix.eval.Task
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}

import scala.scalajs.js
import scala.scalajs.js.WrappedArray

// Operations on a Storage with F effects
// To abstract over text key-value storage that has different effects performed by its operations
// Examples of uses:
// localStorage, sessionStorage: use F = Task
// pure: Use F = State[Map[String, String], ?]]
trait Storage[F[_]] {
  def apply(key: String): F[Option[String]]

  def update(key: String, data: String): F[Unit]

  def remove(key: String): F[Unit]
}

trait LsStorage[F[_]] {
  val lsKeys: F[WrappedArray[String]]
}

final case class LoggedKeyStorage[F[_] : Monad](underlying: Storage[F]) extends Storage[WriterT[F, Vector[String], ?]] {
  override def apply(key: String): WriterT[F, Vector[String], Option[String]] =
    WriterT(underlying(key).map(v => (key +: Vector.empty, v)))

  override def update(key: String, data: String): WriterT[F, Vector[String], Unit] =
    WriterT(underlying.update(key, data).map(v => (key +: Vector.empty, v)))

  override def remove(key: String): WriterT[F, Vector[String], Unit] =
    WriterT.lift(underlying.remove(key))
}

final case class LoggedActionStorage[F[_] : Applicative](underlying: Storage[F]) extends Storage[WriterT[F, Vector[String], ?]] {
  override def apply(key: String): WriterT[F, Vector[String], Option[String]] =
    WriterT(underlying(key).map(v => (s"Get($key)" +: Vector.empty, v)))

  override def update(key: String, data: String): WriterT[F, Vector[String], Unit] =
    WriterT(underlying.update(key, data).map(v => (s"Update($key, $data)" +: Vector.empty, v)))

  override def remove(key: String): WriterT[F, Vector[String], Unit] =
    WriterT(underlying.remove(key).map(v => (s"Remove($key)" +: Vector.empty, v)))
}

// Implementation for dom.ext.Storage values
final class DomStorage(underlying: SStorage) extends Storage[Task] with LsStorage[Task] {
  override def apply(key: String): Task[Option[String]] =
    Task.eval(underlying(key))

  override def update(key: String, data: String): Task[Unit] =
    Task.eval(underlying.update(key, data))

  override def remove(key: String): Task[Unit] =
    Task.eval(underlying.remove(key))

  override val lsKeys: Task[WrappedArray[String]] =
    Task.eval {
      var i = 0
      val arr: js.Array[String] = js.Array()
      val len = underlying.length
      while (i != len) {
        arr.push(underlying.key(i).get)
        i += 1
      }
      new WrappedArray(arr)
    }

}

object DomStorage {

  val Local: DomStorage = new DomStorage(LocalStorage)

  val Session: DomStorage = new DomStorage(SessionStorage)

}

// Implementation for pure maps
object PureStorage extends Storage[StringMapState] with LsStorage[StringMapState] {

  override def apply(key: String): StringMapState[Option[String]] =
    State.inspect(_.get(key))

  override def update(key: String, data: String): StringMapState[Unit] =
    State.modify(_ + (key -> data))

  override def remove(key: String): StringMapState[Unit] =
    State.modify(_ - key)

  override val lsKeys: StringMapState[WrappedArray[String]] =
    State.inspect(_.keysIterator.to[WrappedArray])

}

