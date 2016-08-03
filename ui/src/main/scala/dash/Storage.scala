package dash

import monix.eval.Task
import org.scalajs.dom.ext.{LocalStorage, SessionStorage, Storage => SStorage}

case class Storage(underlying: SStorage) extends AnyVal {

  import Task.evalAlways

  def length: Task[Int] = evalAlways(underlying.length)

  def apply(key: String): Task[Option[String]] = evalAlways(underlying.apply(key))

  def update(key: String, data: String): Task[Unit] = evalAlways(underlying.update(key, data))

  def clear(): Task[Unit] = evalAlways(underlying.clear())

  def remove(key: String): Task[Unit] = evalAlways(underlying.remove(key))

  def key(index: Int): Task[Option[String]] = evalAlways(underlying.key(index))

}

object Storage {
  val Local = Storage(LocalStorage)
  val Session = Storage(SessionStorage)
}